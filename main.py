import sys
from PySide6.QtWidgets import QApplication, QWidget, QVBoxLayout, QSlider, QLabel, QHBoxLayout, QCheckBox, QPushButton, QGridLayout, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QSpinBox, QLineEdit, QColorDialog, QFileDialog, QComboBox, QScrollArea, QGroupBox, QRadioButton, QButtonGroup, QMessageBox, QListWidget, QListWidgetItem, QAbstractItemView
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QPainter, QPixmap, QPen
from pyswip import Prolog
import random
import json
import math
MAX_HEIGHT = 150
MAX_WIDTH = 150
MIN_HEIGHT = 50
MIN_WIDTH = 50
class TileSet:
	def __init__(self, name, traversal_cost, cannot_be_next_to, must_be_next_to, color, discoverable, texture_path):
		self.name = name
		self.traversal_cost = traversal_cost
		self.cannot_be_next_to = cannot_be_next_to
		self.must_be_next_to = must_be_next_to
		self.color = color
		self.discoverable = discoverable
		self.texture_path = texture_path

	def to_dict(self):
		return {
			'name': self.name,
			'traversal_cost': self.traversal_cost,
			'cannot_be_next_to': self.cannot_be_next_to,
			'must_be_next_to': self.must_be_next_to,
			'color': self.color.name(),
			'discoverable' : self.discoverable, # if this is true, then scroll bar in widget will appear
			'texture_path': self.texture_path
		}

	@staticmethod
	def from_dict(data):
		return TileSet(
			data['name'],
			data['traversal_cost'],
			data['cannot_be_next_to'],
			data['must_be_next_to'],
			QColor(data['color']),
			data['discoverable'],
			data['texture_path']
		)

class TileStructure:
	# Each member must know its neighbors of every direction
	def __init__(self, name, north=None, south=None, east=None, west=None):
		self.name = name
		self.north = north
		self.south = south
		self.east = east
		self.west = west

	def __str__(self):
		return self.name

	def __repr__(self):
		return self.name

	def __eq__(self, other):
		return self.name == other.name

	def assign_neighbors(self, north, south, east, west):
		self.north = north
		self.south = south
		self.east = east
		self.west = west


class GraphicViewOverloader(QGraphicsView):
	def __init__(self, parent=None):
		super().__init__(parent)
		self.setRenderHint(QPainter.Antialiasing)
		self.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
		self.setResizeAnchor(QGraphicsView.AnchorUnderMouse)
		self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
		self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
		self.setInteractive(True)
		self.setViewportUpdateMode(QGraphicsView.FullViewportUpdate)
		self.drag_active = False
		self.drawing_active = False
		self.last_pos = None

	def mousePressEvent(self, event):
		parent = self.parent()
		if event.button() == Qt.LeftButton:
			if hasattr(parent, 'seeded_radio') and parent.seeded_radio.isChecked() and parent.seeded_seeds is not None:
				self.drawing_active = True
				if hasattr(parent, 'handle_map_click'):
					parent.handle_map_click(event)
			elif hasattr(parent, 'path_mode_checkbox') and parent.path_mode_checkbox.isChecked() and parent.current_map_data:
				# Allow pathfinding clicks to propagate
				if hasattr(parent, 'handle_map_click'):
					parent.handle_map_click(event)
			else:
				# Enable dragging for non-drawing, non-pathfinding clicks
				self.setDragMode(QGraphicsView.ScrollHandDrag)
				self.drag_active = True
				self.last_pos = event.position()
				super().mousePressEvent(event)

	def mouseMoveEvent(self, event):
		parent = self.parent()
		if self.drawing_active:
			if hasattr(parent, 'seeded_radio') and parent.seeded_radio.isChecked() and parent.seeded_seeds is not None:
				if hasattr(parent, 'handle_map_click'):
					parent.handle_map_click(event)
		elif self.drag_active:
			super().mouseMoveEvent(event)
		else:
			super().mouseMoveEvent(event)

	def mouseReleaseEvent(self, event):
		if event.button() == Qt.LeftButton:
			if self.drawing_active:
				self.drawing_active = False
		if self.drag_active:
			self.setDragMode(QGraphicsView.NoDrag)
			self.drag_active = False
		super().mouseReleaseEvent(event)

	def wheelEvent(self, event):
		zoom_in_factor = 1.2
		zoom_out_factor = 1 / zoom_in_factor

		if event.angleDelta().y() > 0:
			zoom_factor = zoom_in_factor
		else:
			zoom_factor = zoom_out_factor

		self.scale(zoom_factor, zoom_factor)

class TileSetEditor(QWidget):
	def __init__(self, tile_sets, parent=None):
		super().__init__(parent)
		self.tile_sets = tile_sets
		self.selected_color = QColor(Qt.white)
		self.is_new_tile = False
		self.initUI()

	def initUI(self):
		main_layout = QVBoxLayout()
		
		# Create a scroll area for the entire editor
		scroll_area = QScrollArea()
		scroll_area.setWidgetResizable(True)
		scroll_widget = QWidget()
		layout = QVBoxLayout(scroll_widget)

		# Tile Set Selection
		selection_layout = QHBoxLayout()
		self.tile_set_list = QComboBox()
		self.tile_set_list.addItems([tile_set.name for tile_set in self.tile_sets])
		self.tile_set_list.currentIndexChanged.connect(self.load_tile_set)
		selection_layout.addWidget(QLabel('Select Tile Set:'))
		selection_layout.addWidget(self.tile_set_list)
		layout.addLayout(selection_layout)

		# Grid layout for form fields
		form_layout = QGridLayout()
		current_row = 0

		# Name
		form_layout.addWidget(QLabel('Tile Name:'), current_row, 0)
		self.tile_set_name = QLineEdit()
		form_layout.addWidget(self.tile_set_name, current_row, 1)
		current_row += 1

		# Traversal Cost
		form_layout.addWidget(QLabel('Traversal Cost:'), current_row, 0)
		self.tile_set_traversal_cost = QSpinBox()
		self.tile_set_traversal_cost.setRange(1, 100)
		form_layout.addWidget(self.tile_set_traversal_cost, current_row, 1)
		current_row += 1

		# Cannot Be Next To
		form_layout.addWidget(QLabel('Cannot Be Next To:'), current_row, 0)
		self.tile_set_cannot_be_next_to = QListWidget()
		self.tile_set_cannot_be_next_to.setSelectionMode(QAbstractItemView.NoSelection)
		self.tile_set_cannot_be_next_to.setFixedHeight(100)
		for ts in self.tile_sets:
			item = QListWidgetItem(ts.name)
			item.setFlags(item.flags() | Qt.ItemIsUserCheckable)
			item.setCheckState(Qt.Unchecked)
			self.tile_set_cannot_be_next_to.addItem(item)
		form_layout.addWidget(self.tile_set_cannot_be_next_to, current_row, 1)
		current_row += 1

		# Must Be Next To
		form_layout.addWidget(QLabel('Must Be Next To:'), current_row, 0)
		self.tile_set_must_be_next_to = QListWidget()
		self.tile_set_must_be_next_to.setSelectionMode(QAbstractItemView.NoSelection)
		self.tile_set_must_be_next_to.setFixedHeight(100)
		for ts in self.tile_sets:
			item = QListWidgetItem(ts.name)
			item.setFlags(item.flags() | Qt.ItemIsUserCheckable)
			item.setCheckState(Qt.Unchecked)
			self.tile_set_must_be_next_to.addItem(item)
		form_layout.addWidget(self.tile_set_must_be_next_to, current_row, 1)
		current_row += 1

		# Color
		form_layout.addWidget(QLabel('Color:'), current_row, 0)
		color_layout = QHBoxLayout()
		self.tile_set_color = QPushButton()
		self.tile_set_color.setFixedSize(50, 25)
		self.tile_set_color.clicked.connect(self.select_color)
		self.color_hex = QLineEdit()
		self.color_hex.setReadOnly(True)
		color_layout.addWidget(self.tile_set_color)
		color_layout.addWidget(self.color_hex)
		form_layout.addLayout(color_layout, current_row, 1)
		current_row += 1

		# Discoverable
		form_layout.addWidget(QLabel('Discoverable:'), current_row, 0)
		self.tile_set_discoverable = QCheckBox()
		form_layout.addWidget(self.tile_set_discoverable, current_row, 1)
		current_row += 1

		# Texture Path
		form_layout.addWidget(QLabel('Texture Path:'), current_row, 0)
		texture_layout = QHBoxLayout()
		self.tile_set_texture_path = QLineEdit()
		self.select_texture_button = QPushButton('Browse...')
		self.select_texture_button.clicked.connect(self.select_texture_path)
		texture_layout.addWidget(self.tile_set_texture_path)
		texture_layout.addWidget(self.select_texture_button)
		form_layout.addLayout(texture_layout, current_row, 1)

		layout.addLayout(form_layout)

		# Buttons
		button_layout = QHBoxLayout()
		new_button = QPushButton('New Tile')
		new_button.clicked.connect(self.new_tile)
		save_button = QPushButton('Save Changes')
		save_button.clicked.connect(self.save_changes)
		cancel_button = QPushButton('Cancel')
		cancel_button.clicked.connect(self.close_overwrite)
		button_layout.addWidget(new_button)
		button_layout.addWidget(save_button)
		button_layout.addWidget(cancel_button)
		layout.addLayout(button_layout)

		# Add stretch to push everything to the top
		layout.addStretch()

		scroll_area.setWidget(scroll_widget)
		main_layout.addWidget(scroll_area)
		self.setLayout(main_layout)
		
		# Set window properties
		self.setWindowTitle('Edit Tile Sets')
		self.setMinimumWidth(400)
		self.setMinimumHeight(500)
		
		# Load the first tile set
		if self.tile_sets:
			self.load_tile_set(0)

	def load_tile_set(self, index):
		if index >= 0 and index < len(self.tile_sets):
			self.is_new_tile = False
			tile_set = self.tile_sets[index]
			self.tile_set_name.setText(tile_set.name)
			self.tile_set_traversal_cost.setValue(tile_set.traversal_cost)

			for i in range(self.tile_set_cannot_be_next_to.count()):
				item = self.tile_set_cannot_be_next_to.item(i)
				if item.text() in tile_set.cannot_be_next_to:
					item.setCheckState(Qt.Checked)
				else:
					item.setCheckState(Qt.Unchecked)

			for i in range(self.tile_set_must_be_next_to.count()):
				item = self.tile_set_must_be_next_to.item(i)
				if item.text() in tile_set.must_be_next_to:
					item.setCheckState(Qt.Checked)
				else:
					item.setCheckState(Qt.Unchecked)

			self.selected_color = tile_set.color
			self.update_color_display(tile_set.color)
			self.tile_set_discoverable.setChecked(tile_set.discoverable)
			self.tile_set_texture_path.setText(tile_set.texture_path)

	def update_color_display(self, color):
		self.tile_set_color.setStyleSheet(f'background-color: {color.name()}')
		self.color_hex.setText(color.name())

	def select_color(self):
		if not self.selected_color:
			self.selected_color = QColor(0, 0, 0)
		color = QColorDialog.getColor(self.selected_color)
		if color.isValid():
			self.selected_color = color
			self.update_color_display(color)

	def select_texture_path(self):
		file_dialog = QFileDialog()
		file_path, _ = file_dialog.getOpenFileName(
			self, 
			"Select Texture Path", 
			"", 
			"Image Files (*.png *.jpg *.bmp)"
		)
		if file_path:
			self.tile_set_texture_path.setText(file_path)

	def new_tile(self):
		self.tile_set_name.clear()
		self.tile_set_traversal_cost.setValue(1)
		self.tile_set_cannot_be_next_to.clear()
		self.tile_set_must_be_next_to.clear()
		self.selected_color = QColor(0, 0, 0)
		self.update_color_display(self.selected_color)
		self.tile_set_discoverable.setChecked(False)
		self.tile_set_texture_path.clear()

		#for i in range(self.tile_set_cannot_be_next_to.count()):
		#	item = self.tile_set_cannot_be_next_to.item(i)
		#	item.setCheckState(Qt.Unchecked)

		#for i in range(self.tile_set_must_be_next_to.count()):
		#	item = self.tile_set_must_be_next_to.item(i)
		#	item.setCheckState(Qt.Unchecked)

		self.is_new_tile = True

		self.tile_set_list.blockSignals(True)
		self.tile_set_list.setCurrentIndex(-1)
		self.tile_set_list.blockSignals(False)

	def close_overwrite(self):
		if hasattr(self.parent(), 'refresh_ui'):
			self.parent().refresh_ui()
		self.close()

	def save_changes(self):
		# Get data from input fields
		name = self.tile_set_name.text().strip()
		if not name:
			QMessageBox.warning(self, "Validation Error", "Tile name cannot be empty.")
			return

		traversal_cost = self.tile_set_traversal_cost.value()
		
		cannot_be_next_to = []
		for i in range(self.tile_set_cannot_be_next_to.count()):
			item = self.tile_set_cannot_be_next_to.item(i)
			if item.checkState() == Qt.Checked:
				cannot_be_next_to.append(item.text())

		must_be_next_to = []
		for i in range(self.tile_set_must_be_next_to.count()):
			item = self.tile_set_must_be_next_to.item(i)
			if item.checkState() == Qt.Checked:
				must_be_next_to.append(item.text())


		color = self.selected_color
		discoverable = self.tile_set_discoverable.isChecked()
		texture_path = self.tile_set_texture_path.text().strip()

		# Check for unique tile name
		existing_names = [ts.name for ts in self.tile_sets]
		if self.is_new_tile and name in existing_names:
			QMessageBox.warning(self, "Validation Error", "Tile name must be unique.")
			return

		if getattr(self, 'is_new_tile', False):
			# Create a new TileSet instance
			new_tile_set = TileSet(
				name,
				traversal_cost,
				cannot_be_next_to,
				must_be_next_to,
				color,
				discoverable,
				texture_path
			)
			# Add the new tile set to the list
			self.tile_sets.append(new_tile_set)
			# Update the tile set combo box
			self.tile_set_list.addItem(name)
			index = self.tile_set_list.count() - 1
			self.tile_set_list.setCurrentIndex(index)
			self.is_new_tile = False
		else:
			# Update existing tile set
			index = self.tile_set_list.currentIndex()
			if index < 0 or index >= len(self.tile_sets):
				QMessageBox.warning(self, "Selection Error", "No tile set selected.")
				return

			tile_set = self.tile_sets[index]

			# Update tile set properties
			tile_set.name = name
			tile_set.traversal_cost = traversal_cost
			tile_set.cannot_be_next_to = cannot_be_next_to
			tile_set.must_be_next_to = must_be_next_to
			tile_set.color = color
			tile_set.discoverable = discoverable
			tile_set.texture_path = texture_path

			# Update the combo box if the name has changed
			self.tile_set_list.setItemText(index, name)

		# Save to JSON file
		if hasattr(self.parent(), 'save_tile_sets'):
			self.parent().save_tile_sets("tile_sets.json")

		QMessageBox.information(self, "Success", "Tile set saved successfully.")
class MapGenerator(QWidget):
	def __init__(self):
		super().__init__()

		self.prolog = Prolog()

		self.tile_sets = []
		self.locked_tiles = {}
		self.load_tile_sets("tile_sets.json")
		self.json_to_prolog("tile_sets.json", "tile_sets.pl")
		
		self.prolog.consult("map_rules2.pl")
		self.prolog.consult("map_rules3.pl")
		self.prolog.consult("constraints.pl")
		self.prolog.consult("pathfinding.pl")
		#self.prolog.consult("wfc.pl")
		
		self.start_pos = None
		self.end_pos = None
		self.current_path = None
		self.current_map_data = None
		self.cell_size = None
		
		self.initUI()

	def initUI(self):
		layout = QHBoxLayout()  # Change to QHBoxLayout for horizontal layout

		# Left panel (controls)
		left_layout = QVBoxLayout()
		self.left_layout = left_layout
		self.left_layout_initial_count = self.left_layout.count()
		self.percentage_sliders = {}
		self.percentage_labels = {}
		total_percentage = 0

		# Generation switch
		method_label = QLabel('Generation Method:')
		self.seeded_radio = QRadioButton('Seeded Fillings')
		self.perlin_radio = QRadioButton('Perlin Noise')
		self.random_radio = QRadioButton('Pure Random')
		self.perlin_radio.setChecked(True)  # Set Perlin as default
		method_group = QButtonGroup(self)
		method_group.addButton(self.seeded_radio)
		method_group.addButton(self.perlin_radio)
		method_group.addButton(self.random_radio)
		left_layout.addWidget(method_label)
		left_layout.addWidget(self.seeded_radio)
		left_layout.addWidget(self.perlin_radio)
		left_layout.addWidget(self.random_radio)

		for tile_set in self.tile_sets:
			if not tile_set.discoverable:
				continue
			label = QLabel(f'{tile_set.name.capitalize()}')
			slider = QSlider(Qt.Horizontal)
			slider.setRange(0, 100)
			slider.setValue(0)
			slider.valueChanged.connect(self.update_labels)
			left_layout.addWidget(label)
			left_layout.addWidget(slider)
			self.percentage_labels[tile_set.name] = label
			self.percentage_sliders[tile_set.name] = slider

		# Seed Label and Button
		self.seed_label = QLabel('Seed: 0')
		self.regenerate_button = QPushButton('Regenerate Seed')
		self.regenerate_button.clicked.connect(self.regenerate_seed)
		left_layout.addWidget(self.seed_label)
		left_layout.addWidget(self.regenerate_button)

		# Width and Height Sliders
		self.width_label = QLabel(f'Width: {MIN_WIDTH}')
		self.width_slider = QSlider(Qt.Horizontal)
		self.width_slider.setRange(MIN_WIDTH, MAX_WIDTH)
		self.width_slider.setValue(MIN_WIDTH)
		self.width_slider.valueChanged.connect(self.update_labels)
		left_layout.addWidget(self.width_label)
		left_layout.addWidget(self.width_slider)

		self.height_label = QLabel(f'Height: {MIN_HEIGHT}')
		self.height_slider = QSlider(Qt.Horizontal)
		self.height_slider.setRange(MIN_HEIGHT, MAX_HEIGHT)
		self.height_slider.setValue(MIN_HEIGHT)
		self.height_slider.valueChanged.connect(self.update_labels)
		left_layout.addWidget(self.height_label)
		left_layout.addWidget(self.height_slider)

		# Auto Adjust Checkbox
		self.auto_adjust_checkbox = QCheckBox('Auto Adjust Sliders')
		left_layout.addWidget(self.auto_adjust_checkbox)

		# Generate Button
		self.generate_button = QPushButton('Generate Map')
		self.generate_button.clicked.connect(self.generate_map)
		left_layout.addWidget(self.generate_button)

		# Add Pathfinding Controls
		pathfinding_group = QGroupBox("Pathfinding Controls")
		pathfinding_layout = QVBoxLayout()
	
		# Path Selection Mode
		self.path_mode_checkbox = QCheckBox("Path Selection Mode")
		self.path_mode_checkbox.setToolTip("When checked, click on the map to set start and end points")
		pathfinding_layout.addWidget(self.path_mode_checkbox)
		
		# Clear Path Button
		self.clear_path_button = QPushButton("Clear Path")
		self.clear_path_button.clicked.connect(self.clear_path)
		pathfinding_layout.addWidget(self.clear_path_button)
		
		# Path Cost Display
		self.path_cost_label = QLabel("Path Cost: N/A")
		pathfinding_layout.addWidget(self.path_cost_label)
		
		pathfinding_group.setLayout(pathfinding_layout)
		left_layout.addWidget(pathfinding_group)

		# Edit Tile Sets Button
		self.edit_button = QPushButton('Edit Tile Sets')
		self.edit_button.clicked.connect(self.open_tile_set_editor)
		left_layout.addWidget(self.edit_button)

		self.seeded_radio.toggled.connect(self.on_generation_method_changed)
		self.paint_tile_combo = None
		self.clear_seeds_button = None


		# Add the left panel to the main layout
		layout.addLayout(left_layout)

		#Update map_display initialization
		self.map_display = GraphicViewOverloader()
		self.map_display.setFixedSize(500, 500)
		self.map_display.setRenderHint(QPainter.Antialiasing)
		self.map_display.mousePressEvent = self.handle_map_click

		# Right panel (map display)
		right_layout = QVBoxLayout()
		self.map_display = GraphicViewOverloader(self)
		self.map_display.setFixedSize(500, 500)
		self.map_display.setRenderHint(QPainter.Antialiasing)
		self.map_display.setDragMode(QGraphicsView.ScrollHandDrag)
		self.map_display.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
		self.map_display.setResizeAnchor(QGraphicsView.AnchorUnderMouse)
		self.map_display.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
		self.map_display.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
		self.map_display.setInteractive(True)
		self.map_display.setViewportUpdateMode(QGraphicsView.FullViewportUpdate)
		self.scene = QGraphicsScene(self)
		self.map_display.setScene(self.scene)
		right_layout.addWidget(self.map_display)

		# Zoom In and Zoom Out Buttons
		zoom_layout = QHBoxLayout()
		self.zoom_in_button = QPushButton('Zoom In')
		self.zoom_in_button.clicked.connect(self.zoom_in)
		zoom_layout.addWidget(self.zoom_in_button)

		self.zoom_out_button = QPushButton('Zoom Out')
		self.zoom_out_button.clicked.connect(self.zoom_out)
		zoom_layout.addWidget(self.zoom_out_button)

		right_layout.addLayout(zoom_layout)

		# Add the right panel to the main layout
		layout.addLayout(right_layout)

		self.setLayout(layout)
		self.setWindowTitle('Map Generator')

	def find_and_draw_path(self):
		if not self.start_pos or not self.end_pos or not self.current_map_data:
			print("[DEBUG] Missing required data for pathfinding")
			print(f"[DEBUG] Start position: {self.start_pos}")
			print(f"[DEBUG] End position: {self.end_pos}")
			print(f"[DEBUG] Map data exists: {bool(self.current_map_data)}")
			return

		print("\n=== PATHFINDING DEBUG INFO ===")
		print(f"[DEBUG] Starting pathfinding from {self.start_pos} to {self.end_pos}")
		
		# Convert map data to Prolog list format with atoms
		map_str = '[' + ', '.join(
			'[' + ', '.join(tile for tile in row) + ']'
			for row in self.current_map_data
		) + ']'
		print("\n[DEBUG] Formatted map data for Prolog:")
		print(map_str)

		# Format positions as pos(X, Y)
		start_formatted = f"pos({self.start_pos[0]}, {self.start_pos[1]})"
		end_formatted = f"pos({self.end_pos[0]}, {self.end_pos[1]})"
		print(f"\n[DEBUG] Formatted start position: {start_formatted}")
		print(f"[DEBUG] Formatted end position: {end_formatted}")

		# Create Prolog query
		query = f"find_path({map_str}, {start_formatted}, {end_formatted}, Path)"
		print("\n[DEBUG] Generated Prolog query:")
		print(query)

		try:
			print("\n[DEBUG] Executing Prolog query...")
			# Execute query
			result = list(self.prolog.query(query))
			print(f"[DEBUG] Query result: {result}")

			if result:
				print("\n[DEBUG] Path found! Processing results...")
				# Extract path from result and convert to coordinate tuples
				path_str = result[0]['Path']
				print(f"[DEBUG] Raw path from Prolog: {path_str}")

				# Convert 'pos(X,Y)' strings to coordinate tuples
				self.current_path = []
				for pos in path_str:
					# Extract numbers from pos string using string manipulation
					nums = pos.replace('pos(', '').replace(')', '').split(',')
					x, y = int(nums[0]), int(nums[1])
					self.current_path.append((x, y))
					print(f"[DEBUG] Processed position: ({x}, {y})")

				if self.current_path:
					print("\n[DEBUG] Path conversion successful")
					# Format the Path for Prolog query
					path_formatted = '[' + ', '.join(
						f"pos({x}, {y})" for x, y in self.current_path
					) + ']'
					print(f"[DEBUG] Formatted path for cost calculation: {path_formatted}")

					# Get path cost
					cost_query = f"find_path_cost({path_formatted}, {map_str}, Cost)"
					print("\n[DEBUG] Executing cost calculation query:")
					print(cost_query)
					
					cost_result = list(self.prolog.query(cost_query))
					print(f"[DEBUG] Cost calculation result: {cost_result}")

					if cost_result:
						cost = cost_result[0]['Cost']
						print(f"[DEBUG] Final path cost: {cost}")
						self.path_cost_label.setText(f"Path Cost: {cost}")
					else:
						print("[DEBUG] Failed to calculate path cost")

					self.redraw_map()
					print("\n[DEBUG] Map redraw completed")
				else:
					print("[DEBUG] Failed to parse path coordinates")
					self.path_cost_label.setText("Error parsing path!")

			else:
				print("[DEBUG] No path found in query result")
				self.path_cost_label.setText("No path found!")

		except Exception as e:
			print("\n=== PATHFINDING ERROR ===")
			print(f"[DEBUG] Error in pathfinding: {e}")
			print(f"[DEBUG] Exception type: {type(e)}")
			print(f"[DEBUG] Exception details: {str(e)}")
			import traceback
			print("[DEBUG] Full traceback:")
			print(traceback.format_exc())
			self.path_cost_label.setText("Error finding path!")

		print("\n=== END PATHFINDING DEBUG INFO ===\n")

	def clear_path(self):
		self.start_pos = None
		self.end_pos = None
		self.current_path = None
		self.path_cost_label.setText("Path Cost: N/A")
		self.redraw_map()

	def zoom_in(self):
		self.map_display.scale(1.2, 1.2)

	def zoom_out(self):
		self.map_display.scale(1 / 1.2, 1 / 1.2)

	#def update_labels(self):
	#	total = sum(slider.value() for slider in self.percentage_sliders.values())
	#	if total > 100 and self.auto_adjust_checkbox.isChecked():
	#		excess = total - 100
	#		for slider in self.percentage_sliders.values():
	#			if slider.value() > excess:
	#				slider.setValue(slider.value() - excess)
	#				break

	#	for name, slider in self.percentage_sliders.items():
	#		self.percentage_labels[name].setText(f'{name.capitalize()} Percentage: {slider.value()}%')

	#	self.width_label.setText(f'Width: {self.width_slider.value()}')
	#	self.height_label.setText(f'Height: {self.height_slider.value()}')

	# This one regressed so that it doesn't perform push back
	def update_labels(self):
		sender = self.sender()
		if sender in self.percentage_sliders.values():
			total_other = sum(slider.value() for name, slider in self.percentage_sliders.items() if slider != sender)
			max_value = 100 - total_other
			if sender.value() > max_value:
				sender.blockSignals(True)
				sender.setValue(max_value)
				sender.blockSignals(False)

		for name, slider in self.percentage_sliders.items():
			self.percentage_labels[name].setText(f'{name.capitalize()} Percentage: {slider.value()}%')

		self.width_label.setText(f'Width: {self.width_slider.value()}')
		self.height_label.setText(f'Height: {self.height_slider.value()}')

	def regenerate_seed(self):
		#result = list(self.prolog.query("random_seed(Seed)"))
		#if result:
		#    seed = result[0]['Seed']
		#    self.seed_label.setText(f'Seed: {seed}')

		# Temporary solution
		seed = random.randint(0, 1000000)
		self.seed_label.setText(f'Seed: {seed}')

	def generate_map(self):
		width = self.width_slider.value()
		height = self.height_slider.value()
		percentages = {name: slider.value() for name, slider in self.percentage_sliders.items()}

		if sum(percentages.values()) != 100:
			QMessageBox.warning(self, "Percentage Error", "The sum of all percentages must be 100.")
			return

		for slider in self.percentage_sliders.values():
			slider.setEnabled(True)
		self.locked_tiles.clear()

		if self.seeded_radio.isChecked():
			map_data = []
			for y in range(height):
				row = []
				for x in range(width):
					tile_name = self.seeded_seeds[y][x]
					if tile_name:
						row.append(tile_name)
					else:
						row.append(None)
				map_data.append(row)

			# Fill empty tiles
			# Create a list of current seeded tiles in the map_data
			seeded_tiles = set(tile for row in map_data for tile in row if tile)
			print(f"Seeded tiles: {seeded_tiles}")
			#print(map_data)
			for tile in seeded_tiles:
				percentages[tile] = 0

			#normalise the percentages that has value to combine into 100 in total
			percentages = {name: value for name, value in percentages.items() if value > 0}
			percentages_sum = sum(percentages.values())
			percentages = {name: value / percentages_sum * 100 for name, value in percentages.items()}
			print(f"Normalised percentages: {percentages}")

			self.fill_empty_tiles(map_data, percentages)
			#map_data = self.correct_map(map_data)
			self.current_map_data = map_data
			self.redraw_map()
		elif self.perlin_radio.isChecked():
			percentages_list = [f'{name}-{percentages[name]}' for name in percentages if percentages[name] > 0]
			query = f"generate_map({width}, {height}, [{', '.join(percentages_list)}], Map)"
			result = list(self.prolog.query(query))
			if result:
				map_data = result[0]['Map']
				map_data = self.correct_map(map_data)
				self.display_map(map_data)
			else:
				print("No result from Prolog query")
		else:
			percentages_list = [f'{name}-{percentages[name]}' for name in percentages if percentages[name] > 0]
			query = f"legacy_generate_map({width}, {height}, [{', '.join(percentages_list)}], Map)"
			result = list(self.prolog.query(query))
			if result:
				map_data = result[0]['Map']
				self.display_map(map_data)
			else:
				print("No result from Prolog query")

	def correct_map(self, map_data):
		height = len(map_data)
		width = len(map_data[0])
		corrected_map = [row[:] for row in map_data]
		for y in range(height):
			for x in range(width):
				current_tile_name = corrected_map[y][x]
				current_tile_set = next((ts for ts in self.tile_sets if ts.name == current_tile_name), None)
				if not current_tile_set:
					continue
				neighbor_positions = []
				if y > 0:
					neighbor_positions.append((x, y - 1))
				if y < height - 1:
					neighbor_positions.append((x, y + 1))
				if x > 0:
					neighbor_positions.append((x - 1, y))
				if x < width - 1:
					neighbor_positions.append((x + 1, y))
				neighbors = [corrected_map[ny][nx] for nx, ny in neighbor_positions]

				# Check 'Cannot Be Next To' constraints
				#invalid_neighbor = any(
				#	neighbor in current_tile_set.cannot_be_next_to
				#	for neighbor in neighbors
				#)
				neighbors_str = '[' + ', '.join(f"'{neighbor}'" for neighbor in neighbors) + ']'
				query = f"invalid_tile('{current_tile_name}', {neighbors_str})"
				result = list(self.prolog.query(query))
				
				invalid_neighbor = bool(result)

				if invalid_neighbor:
					suitable_tile_found = False
					# Try non-discoverable tiles
					for border_tile in self.tile_sets:
						if not border_tile.discoverable:
							query_border = f"border_invalid('{border_tile.name}', {neighbors_str})"
							result_border = list(self.prolog.query(query_border))
							border_invalid = bool(result_border)
							if not border_invalid:
								corrected_map[y][x] = border_tile.name
								suitable_tile_found = True
								break
					# Try discoverable tiles
					if not suitable_tile_found:
						for ts in self.tile_sets:
							ts_invalid = any(
								neighbor in ts.cannot_be_next_to
								for neighbor in neighbors
							) or (
								ts.must_be_next_to and not any(
									neighbor in ts.must_be_next_to
									for neighbor in neighbors
								)
							)
							if not ts_invalid:
								corrected_map[y][x] = ts.name
								suitable_tile_found = True
								break
					# If no tile is found, leave the tile unchanged
					if not suitable_tile_found:
						corrected_map[y][x] = current_tile_name

				# Check 'Must Be Next To' constraints
				elif current_tile_set.must_be_next_to:
					query_must = f"must_be_next_to('{current_tile_name}', {neighbors_str})"
					result_must = list(self.prolog.query(query_must))
					must_have_neighbor = bool(result_must)
					if not must_have_neighbor:
						suitable_tile_found = False
						# Try non-discoverable tiles
						for border_tile in self.tile_sets:
							if not border_tile.discoverable:
								query_border = f"border_invalid('{border_tile.name}', {neighbors_str})"
								result_border = list(self.prolog.query(query_border))
								border_invalid = bool(result_border)
								if not border_invalid:
									corrected_map[y][x] = border_tile.name
									suitable_tile_found = True
									break
						# Try discoverable tiles
						if not suitable_tile_found:
							for ts in self.tile_sets:
								ts_invalid = any(
									neighbor in ts.cannot_be_next_to
									for neighbor in neighbors
								) or (
									ts.must_be_next_to and not any(
										neighbor in ts.must_be_next_to
										for neighbor in neighbors
									)
								)
								if not ts_invalid:
									corrected_map[y][x] = ts.name
									suitable_tile_found = True
									break
						# If no tile is found, leave the tile unchanged
						if not suitable_tile_found:
							corrected_map[y][x] = current_tile_name
		return corrected_map

	def load_tile_sets(self, filename):
		try:
			with open(filename, 'r') as file:
				data = json.load(file)
				self.tile_sets = [TileSet.from_dict(item) for item in data]
		except FileNotFoundError:
			print(f"Configuration file {filename} not found. Creating default configuration.")
			self.tile_sets = self.create_default_tile_sets()
			self.save_tile_sets(filename)

	def json_to_prolog(self, json_file, prolog_file):
		with open(json_file, 'r') as f:
			tile_sets = json.load(f)

		with open(prolog_file, 'w') as f:
			for tile_set in tile_sets:
				name = tile_set['name']
				traversal_cost = tile_set['traversal_cost']
				cannot_be_next_to = tile_set['cannot_be_next_to']
				must_be_next_to = tile_set['must_be_next_to']
				color = tile_set['color']
				discoverable = 'true' if tile_set['discoverable'] else 'false'
				texture_path = tile_set['texture_path']

				cannot_be_next_to_str = '[' + ', '.join(cannot_be_next_to) + ']'
				must_be_next_to_str = '[' + ', '.join(must_be_next_to) + ']'

				fact = f"tile_set({name}, {traversal_cost}, {cannot_be_next_to_str}, {must_be_next_to_str}, '{color}', {discoverable}, '{texture_path}').\n"
				f.write(fact)

	def display_map(self, map_data):
		self.current_map_data = map_data
		self.redraw_map()

	def redraw_map(self):
		if not self.current_map_data:
			return

		self.scene.clear()
		width = len(self.current_map_data[0])
		height = len(self.current_map_data)
		border_thickness = 2
		self.cell_size = min(
			(self.map_display.width() - border_thickness * 2) // width,
			(self.map_display.height() - border_thickness * 2) // height
		)

		pixmap = QPixmap(width * self.cell_size + border_thickness * 2, height * self.cell_size + border_thickness * 2)
		pixmap.fill(Qt.white)

		painter = QPainter(pixmap)
		try:
			# Draw tiles
			for y, row in enumerate(self.current_map_data):
				for x, cell in enumerate(row):
					tile_set = next((ts for ts in self.tile_sets if ts.name == cell), None)
					if tile_set:
						if tile_set.texture_path:
							texture = QPixmap(tile_set.texture_path)
							painter.drawPixmap(
								x * self.cell_size + border_thickness,
								y * self.cell_size + border_thickness,
								self.cell_size,
								self.cell_size,
								texture
							)
						else:
							color = QColor(tile_set.color)
							painter.fillRect(
								x * self.cell_size + border_thickness,
								y * self.cell_size + border_thickness,
								self.cell_size,
								self.cell_size,
								color
							)
					else:
						painter.fillRect(
							x * self.cell_size + border_thickness,
							y * self.cell_size + border_thickness,
							self.cell_size,
							self.cell_size,
							QColor(0, 0, 0)
						)

			# Draw border
			pen = QPen(Qt.black, border_thickness)
			painter.setPen(pen)
			painter.drawRect(0, 0, pixmap.width() - 1, pixmap.height() - 1)

			# Draw start and end points (if any)
			if self.start_pos:
				painter.setBrush(QColor(255, 0, 0))  # Red for start
				painter.setPen(Qt.NoPen)
				painter.drawEllipse(
					self.start_pos[0] * self.cell_size + self.cell_size // 4 + border_thickness,
					self.start_pos[1] * self.cell_size + self.cell_size // 4 + border_thickness,
					self.cell_size // 2, self.cell_size // 2
				)

			if self.end_pos:
				painter.setBrush(QColor(0, 255, 0))  # Green for end
				painter.setPen(Qt.NoPen)
				painter.drawEllipse(
					self.end_pos[0] * self.cell_size + self.cell_size // 4 + border_thickness,
					self.end_pos[1] * self.cell_size + self.cell_size // 4 + border_thickness,
					self.cell_size // 2, self.cell_size // 2
				)

			# Draw path (if any)
			if self.current_path:
				path_pen = QPen(QColor(255, 255, 255), max(2, self.cell_size // 4))
				path_pen.setCapStyle(Qt.RoundCap)
				painter.setPen(path_pen)

				for i in range(len(self.current_path) - 1):
					x1, y1 = self.current_path[i]
					x2, y2 = self.current_path[i + 1]
					painter.drawLine(
						x1 * self.cell_size + self.cell_size // 2 + border_thickness,
						y1 * self.cell_size + self.cell_size // 2 + border_thickness,
						x2 * self.cell_size + self.cell_size // 2 + border_thickness,
						y2 * self.cell_size + self.cell_size // 2 + border_thickness
					)
		finally:
			painter.end()

		self.scene.addItem(QGraphicsPixmapItem(pixmap))

	def on_editor_closed(self):
		self.edit_button.setEnabled(True)

	def open_tile_set_editor(self):
		self.editor = TileSetEditor(self.tile_sets, self)
		self.editor.setWindowModality(Qt.ApplicationModal)
		self.editor.show()
		self.editor.raise_()
		self.editor.activateWindow()
		self.edit_button.setEnabled(False)

	def select_color(self):
		color = QColorDialog.getColor()
		if color.isValid():
			self.tile_set_color.setStyleSheet(f'background-color: {color.name()}')
			self.selected_color = color

	def select_texture_path(self):
		file_dialog = QFileDialog()
		file_path, _ = file_dialog.getOpenFileName(self, "Select Texture Path", "", "Image Files (*.png *.jpg *.bmp)")
		if file_path:
			self.tile_set_texture_path.setText(file_path)

	def save_tile_sets(self, filename):
		with open(filename, 'w') as file:
			json.dump([tile_set.to_dict() for tile_set in self.tile_sets], file, indent=4)
		self.json_to_prolog(filename, "tile_sets.pl")

	def create_default_tile_sets(self):
		default_tile_sets = [
			TileSet("water", 3, ["land"], [], QColor(0, 0, 255), True, ""),
			TileSet("deep_water", 5, ["sand", "forest", "land"], ["water"], QColor(0, 0, 128), False, ""),
			TileSet("land", 1, [], [], QColor(170, 85, 0), True, ""),
			TileSet("forest", 2, [], [], QColor(0, 128, 0), True, ""),
			TileSet("sand", 1, [], ["water"], QColor(252, 255, 148), False, "")
		]
		return default_tile_sets

	def refresh_ui(self):
		# Re-enable the edit button
		self.edit_button.setEnabled(True)
		
		# Store all static UI elements that should be preserved
		static_elements = [
			# Method selector elements
			(self.left_layout.itemAt(0).widget(), 0),  # Method label
			(self.left_layout.itemAt(1).widget(), 1),  
			(self.left_layout.itemAt(2).widget(), 2),  
			(self.left_layout.itemAt(3).widget(), 3),  
			
			# Control elements (store their original positions)
			(self.seed_label, -8),
			(self.regenerate_button, -7),
			(self.width_label, -6),
			(self.width_slider, -5),
			(self.height_label, -4),
			(self.height_slider, -3),
			(self.auto_adjust_checkbox, -2),
			(self.generate_button, -1)
		]
		
		# Temporarily remove all static elements
		for widget, _ in static_elements:
			self.left_layout.removeWidget(widget)
			widget.setParent(None)  # Detach from layout but preserve widget
			
		# Clear existing percentage sliders and labels
		while self.left_layout.count() > 0:
			item = self.left_layout.takeAt(0)
			if item.widget():
				item.widget().deleteLater()
		
		# Clear the stored sliders and labels
		self.percentage_sliders.clear()
		self.percentage_labels.clear()
		
		# Restore method selector elements first
		for widget, pos in static_elements[:4]:  # First 4 are method selector elements
			self.left_layout.insertWidget(pos, widget)
		
		# Add new sliders and labels for each discoverable tile set
		for tile_set in self.tile_sets:
			if not tile_set.discoverable:
				continue
			
			# Create and add label
			label = QLabel(f'{tile_set.name.capitalize()}')
			self.percentage_labels[tile_set.name] = label
			self.left_layout.addWidget(label)
			
			# Create and add slider
			slider = QSlider(Qt.Horizontal)
			slider.setRange(0, 100)
			slider.setValue(0)
			slider.valueChanged.connect(self.update_labels)
			self.percentage_sliders[tile_set.name] = slider
			self.left_layout.addWidget(slider)
		
		# Restore remaining control elements in their original order
		for widget, _ in static_elements[4:]:  # Skip method selector elements
			self.left_layout.addWidget(widget)
		
		# Add back pathfinding controls
		pathfinding_group = QGroupBox("Pathfinding Controls")
		pathfinding_layout = QVBoxLayout()
		pathfinding_layout.addWidget(self.path_mode_checkbox)
		pathfinding_layout.addWidget(self.clear_path_button)
		pathfinding_layout.addWidget(self.path_cost_label)
		pathfinding_group.setLayout(pathfinding_layout)
		self.left_layout.addWidget(pathfinding_group)
		
		# Add back the edit button
		self.edit_button = QPushButton('Edit Tile Sets')
		self.edit_button.clicked.connect(self.open_tile_set_editor)
		self.left_layout.addWidget(self.edit_button)
		
		# Update labels
		self.update_labels()
		
		# Re-enable painting tool if in 'Seeded Fillings' mode
		if self.seeded_radio.isChecked():
			self.enable_painting_tool()
		else:
			self.disable_painting_tool()
		
		# If there's a current map, redraw it with the updated tile sets
		if self.current_map_data:
			self.redraw_map()


	#3.0

	def on_generation_method_changed(self, checked):
		if self.seeded_radio.isChecked():
			self.enable_painting_tool()
		else:
			self.disable_painting_tool()

	def enable_painting_tool(self):
		if not self.paint_tile_combo:
			self.paint_tile_combo = QComboBox()
			self.paint_tile_combo.addItems([tile_set.name for tile_set in self.tile_sets])
			self.left_layout.insertWidget(4, self.paint_tile_combo)

			self.clear_seeds_button = QPushButton('Clear Seeds')
			self.clear_seeds_button.clicked.connect(self.clear_seeds)
			self.left_layout.insertWidget(5, self.clear_seeds_button)

		else:
			self.paint_tile_combo.show()
			self.clear_seeds_button.show()

		width = self.width_slider.value()
		height = self.height_slider.value()
		self.seeded_seeds = [['' for _ in range(width)] for _ in range(height)]
		self.display_blank_map(width, height)

	def disable_painting_tool(self):
		if self.paint_tile_combo:
			self.paint_tile_combo.hide()
			self.clear_seeds_button.hide()
		self.seeded_seeds = None
		self.scene.clear()

	def handle_map_click(self, event):
		if self.seeded_radio.isChecked() and self.seeded_seeds is not None:
			view_pos = event.position()
			scene_pos = self.map_display.mapToScene(view_pos.toPoint())

			x = int(scene_pos.x() / self.cell_size)
			y = int(scene_pos.y() / self.cell_size)

			if 0 <= x < len(self.seeded_seeds[0]) and 0 <= y < len(self.seeded_seeds):
				tile_name = self.paint_tile_combo.currentText()
				self.seeded_seeds[y][x] = tile_name
				self.display_seed_map()
				self.lock_tile_threshold(tile_name)
				self.adjust_threshold_percentages()
		elif self.path_mode_checkbox.isChecked() and self.current_map_data:
			view_pos = event.position()
			scene_pos = self.map_display.mapToScene(view_pos.toPoint())

			x = int(scene_pos.x() / self.cell_size)
			y = int(scene_pos.y() / self.cell_size)

			if 0 <= x < len(self.current_map_data[0]) and 0 <= y < len(self.current_map_data):
				if not self.start_pos:
					self.start_pos = (x, y)
					self.redraw_map()
				elif not self.end_pos:
					self.end_pos = (x, y)
					self.find_and_draw_path()
				else:
					self.start_pos = (x, y)
					self.end_pos = None
					self.current_path = None
					self.path_cost_label.setText("Path Cost: N/A")
					self.redraw_map()
		else:
			pass


	def lock_tile_threshold(self, tile_name):
		if tile_name not in self.locked_tiles:
			self.locked_tiles[tile_name] = 0
		self.locked_tiles[tile_name] += 1

	def adjust_threshold_percentages(self):
		width = self.width_slider.value()
		height = self.height_slider.value()
		total_tiles = width * height
		for tile_name, count in self.locked_tiles.items():
			percentage = (count / total_tiles) * 100
			self.percentage_sliders[tile_name].setValue(int(percentage))
			self.percentage_sliders[tile_name].setEnabled(False)


	def display_blank_map(self, width, height):
		self.scene.clear()
		self.current_map_data = None

		border_thickness = 2
		self.cell_size = min(
			(self.map_display.width() - border_thickness * 2) // width,
			(self.map_display.height() - border_thickness * 2) // height
		)

		pixmap = QPixmap(width * self.cell_size + border_thickness * 2, height * self.cell_size + border_thickness * 2)
		pixmap.fill(Qt.white)

		painter = QPainter(pixmap)
		try:
			# Draw border
			pen = QPen(Qt.black, border_thickness)
			painter.setPen(pen)
			painter.drawRect(0, 0, pixmap.width() - 1, pixmap.height() - 1)
		finally:
			painter.end()
		self.scene.addItem(QGraphicsPixmapItem(pixmap))

	def display_seed_map(self):
		self.scene.clear()
		width = len(self.seeded_seeds[0])
		height = len(self.seeded_seeds)
		border_thickness = 2
		pixmap = QPixmap(width * self.cell_size + border_thickness * 2, height * self.cell_size + border_thickness * 2)
		pixmap.fill(Qt.white)

		painter = QPainter(pixmap)
		try:
			# Draw tiles
			for y in range(height):
				for x in range(width):
					tile_name = self.seeded_seeds[y][x]
					if tile_name:
						tile_set = next((ts for ts in self.tile_sets if ts.name == tile_name), None)
						if tile_set:
							color = tile_set.color
							painter.fillRect(
								x * self.cell_size + border_thickness,
								y * self.cell_size + border_thickness,
								self.cell_size,
								self.cell_size,
								color
							)
			# Draw border
			pen = QPen(Qt.black, border_thickness)
			painter.setPen(pen)
			painter.drawRect(0, 0, pixmap.width() - 1, pixmap.height() - 1)
		finally:
			painter.end()
		self.scene.addItem(QGraphicsPixmapItem(pixmap))

	def clear_seeds(self):
		width = self.width_slider.value()
		height = self.height_slider.value()
		self.seeded_seeds = [['' for _ in range(width)] for _ in range(height)]

		for slider in self.percentage_sliders.values():
			slider.setEnabled(True)
		for name in self.percentage_sliders:
			self.percentage_sliders[name].setValue(0)
		self.locked_tiles.clear()
		self.display_blank_map(width, height)
		

	def fill_empty_tiles(self, map_data, percentages):
		width = len(map_data[0])
		height = len(map_data)
		total_tiles = width * height

		prefilled_counts = {}
		for row in map_data:
			for cell in row:
				if cell is not None:
					prefilled_counts[cell] = prefilled_counts.get(cell, 0) + 1

		tile_counts = {}
		for tile, percentage in percentages.items():
			count = int(round((percentage / 100.0) * total_tiles))
			tile_counts[tile] = count - prefilled_counts.get(tile, 0)

		tiles_to_fill = []
		for tile, count in tile_counts.items():
			tiles_to_fill.extend([tile] * count)

		random.shuffle(tiles_to_fill)

		idx = 0
		for y in range(height):
			for x in range(width):
				if map_data[y][x] is None and idx < len(tiles_to_fill):
					map_data[y][x] = tiles_to_fill[idx]
					idx += 1

	def propagate(self, wavefunction, x, y):
		stack = [(x, y)]
		width = len(wavefunction[0])
		height = len(wavefunction)
		while stack:
			cx, cy = stack.pop()
			tile = next(iter(wavefunction[cy][cx]))
			neighbors = self.get_neighbors(cx, cy, width, height)
			for nx, ny in neighbors:
				possible_tiles = wavefunction[ny][nx]
				allowed_tiles = set()
				for candidate_tile in possible_tiles:
					if self.are_tiles_compatible(tile, candidate_tile):
						allowed_tiles.add(candidate_tile)
				if allowed_tiles != possible_tiles:
					wavefunction[ny][nx] = allowed_tiles
					if len(allowed_tiles) == 0:
						raise ValueError("No possible tiles left. Constraints are too strict.")
					if len(allowed_tiles) == 1:
						stack.append((nx, ny))

	def get_neighbors(self, x, y, width, height):
		neighbors = []
		if x > 0:
			neighbors.append((x - 1, y))
		if x < width - 1:
			neighbors.append((x + 1, y))
		if y > 0:
			neighbors.append((x, y - 1))
		if y < height - 1:
			neighbors.append((x, y + 1))
		return neighbors

	def are_tiles_compatible(self, tile1, tile2):
		tile1_set = next((ts for ts in self.tile_sets if ts.name == tile1), None)
		tile2_set = next((ts for ts in self.tile_sets if ts.name == tile2), None)
		if tile1_set and tile2_set:
			if tile2 in tile1_set.cannot_be_next_to or tile1 in tile2_set.cannot_be_next_to:
				return False
			if tile1_set.must_be_next_to and tile2 not in tile1_set.must_be_next_to:
				return False
			if tile2_set.must_be_next_to and tile1 not in tile2_set.must_be_next_to:
				return False
		return True

if __name__ == '__main__':
	app = QApplication(sys.argv)
	ex = MapGenerator()
	ex.show()
	sys.exit(app.exec())