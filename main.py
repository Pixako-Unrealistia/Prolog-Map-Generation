import sys
from PySide6.QtWidgets import QApplication, QWidget, QVBoxLayout, QSlider, QLabel, QHBoxLayout, QCheckBox, QPushButton, QGridLayout, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QSpinBox, QLineEdit, QColorDialog, QFileDialog, QComboBox, QScrollArea
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QPainter, QPixmap
from pyswip import Prolog
import random
import json
MAX_HEIGHT = 150
MAX_WIDTH = 150
MIN_HEIGHT = 5
MIN_WIDTH = 5
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

class GraphicViewOverloader(QGraphicsView):
	def __init__(self, parent=None):
		super().__init__(parent)
		self.setRenderHint(QPainter.Antialiasing)
		self.setDragMode(QGraphicsView.ScrollHandDrag)
		self.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
		self.setResizeAnchor(QGraphicsView.AnchorUnderMouse)
		self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
		self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
		self.setInteractive(True)
		self.setViewportUpdateMode(QGraphicsView.FullViewportUpdate)

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
		self.selected_color = None
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
		self.tile_set_cannot_be_next_to = QLineEdit()
		self.tile_set_cannot_be_next_to.setPlaceholderText("Enter comma-separated values")
		form_layout.addWidget(self.tile_set_cannot_be_next_to, current_row, 1)
		current_row += 1

		# Must Be Next To
		form_layout.addWidget(QLabel('Must Be Next To:'), current_row, 0)
		self.tile_set_must_be_next_to = QLineEdit()
		self.tile_set_must_be_next_to.setPlaceholderText("Enter comma-separated values")
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
		save_button = QPushButton('Save Changes')
		save_button.clicked.connect(self.save_changes)
		cancel_button = QPushButton('Cancel')
		cancel_button.clicked.connect(self.close)
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
		if index < 0 or index >= len(self.tile_sets):
			return
			
		tile_set = self.tile_sets[index]
		self.tile_set_name.setText(tile_set.name)
		self.tile_set_traversal_cost.setValue(tile_set.traversal_cost)
		self.tile_set_cannot_be_next_to.setText(','.join(tile_set.cannot_be_next_to))
		self.tile_set_must_be_next_to.setText(','.join(tile_set.must_be_next_to))
		self.selected_color = tile_set.color
		self.update_color_display(tile_set.color)
		self.tile_set_discoverable.setChecked(tile_set.discoverable)
		self.tile_set_texture_path.setText(tile_set.texture_path)

	def update_color_display(self, color):
		self.tile_set_color.setStyleSheet(f'background-color: {color.name()}')
		self.color_hex.setText(color.name())

	def select_color(self):
		color = QColorDialog.getColor(initial=self.selected_color)
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

	def save_changes(self):
		index = self.tile_set_list.currentIndex()
		if index < 0 or index >= len(self.tile_sets):
			return

		tile_set = self.tile_sets[index]
		
		# Update tile set properties
		tile_set.name = self.tile_set_name.text().strip()
		tile_set.traversal_cost = self.tile_set_traversal_cost.value()
		tile_set.cannot_be_next_to = [x.strip() for x in self.tile_set_cannot_be_next_to.text().split(',') if x.strip()]
		tile_set.must_be_next_to = [x.strip() for x in self.tile_set_must_be_next_to.text().split(',') if x.strip()]
		tile_set.color = self.selected_color
		tile_set.discoverable = self.tile_set_discoverable.isChecked()
		tile_set.texture_path = self.tile_set_texture_path.text().strip()

		# Update the combo box if name changed
		current_text = self.tile_set_list.currentText()
		if current_text != tile_set.name:
			self.tile_set_list.setItemText(index, tile_set.name)

		# Save to JSON file
		if hasattr(self.parent(), 'save_tile_sets'):
			self.parent().save_tile_sets("tile_sets.json")

class MapGenerator(QWidget):
	def __init__(self):
		super().__init__()

		self.prolog = Prolog()

		self.tile_sets = []
		self.load_tile_sets("tile_sets.json")
		self.json_to_prolog("tile_sets.json", "tile_sets.pl")

		
		self.prolog.consult("map_rules2.pl")

		self.initUI()

	def initUI(self):
		layout = QHBoxLayout()  # Change to QHBoxLayout for horizontal layout

		# Left panel (controls)
		left_layout = QVBoxLayout()
		self.percentage_sliders = {}
		self.percentage_labels = {}
		total_percentage = 0

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

		# Edit Tile Sets Button
		edit_button = QPushButton('Edit Tile Sets')
		edit_button.clicked.connect(self.open_tile_set_editor)
		left_layout.addWidget(edit_button)

		# Add the left panel to the main layout
		layout.addLayout(left_layout)

		# Right panel (map display)
		right_layout = QVBoxLayout()
		self.map_display = GraphicViewOverloader()
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


	def zoom_in(self):
		self.map_display.scale(1.2, 1.2)

	def zoom_out(self):
		self.map_display.scale(1 / 1.2, 1 / 1.2)

	def update_labels(self):
		total = sum(slider.value() for slider in self.percentage_sliders.values())
		if total > 100 and self.auto_adjust_checkbox.isChecked():
			excess = total - 100
			for slider in self.percentage_sliders.values():
				if slider.value() > excess:
					slider.setValue(slider.value() - excess)
					break

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
			raise ValueError("The sum of all percentages must be 100.")

		percentages_list = [f'{name}-{percentages[name]}' for name in percentages]

		# Call Prolog to generate the map
		query = f"generate_map({width}, {height}, [{', '.join(percentages_list)}], Map)"
		result = list(self.prolog.query(query))

		print(result)

		if result:
			map_data = result[0]['Map']
			self.display_map(map_data)
		else:
			print("No result from Prolog query")

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
		self.scene.clear()
		width = self.width_slider.value()
		height = self.height_slider.value()
		cell_size = min(self.map_display.width() // width, self.map_display.height() // height)
		pixmap = QPixmap(self.map_display.width(), self.map_display.height())
		pixmap.fill(Qt.white)
		
		painter = QPainter(pixmap)
		try:
			for y, row in enumerate(map_data):
				for x, cell in enumerate(row):
					tile_set = next((ts for ts in self.tile_sets if ts.name == cell), None)
					if tile_set:
						color = tile_set.color
					else:
						color = QColor(0, 0, 0)  # Default to black if tile set not found
					painter.fillRect(x * cell_size, y * cell_size, cell_size, cell_size, color)
		finally:
			painter.end()
		
		self.scene.addItem(QGraphicsPixmapItem(pixmap))

	def open_tile_set_editor(self):
		self.editor = TileSetEditor(self.tile_sets, self)
		self.editor.setWindowModality(Qt.ApplicationModal)
		self.editor.show()
		self.editor.raise_()
		self.editor.activateWindow()

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
			TileSet("water", 3, [], [], QColor(0, 0, 255), True, ""),
			TileSet("deep_water", 5, ["sand", "forest", "land"], ["water"], QColor(0, 0, 128), False, ""),
			TileSet("forest", 2, [], [], QColor(0, 128, 0), True, ""),
			TileSet("land", 1, [], [], QColor(255, 255, 0), True, ""),
			TileSet("sand", 1, [], [], QColor(252, 255, 148), False, "")
		]
		return default_tile_sets


if __name__ == '__main__':
	app = QApplication(sys.argv)
	ex = MapGenerator()
	ex.show()
	sys.exit(app.exec())