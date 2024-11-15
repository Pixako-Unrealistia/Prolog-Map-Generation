import sys
from PySide6.QtWidgets import QApplication, QWidget, QVBoxLayout, QSlider, QLabel, QHBoxLayout, QCheckBox, QPushButton, QGridLayout, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QSpinBox, QLineEdit, QColorDialog, QFileDialog, QComboBox, QScrollArea
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QPainter, QPixmap
from pyswip import Prolog
import random
import json
MAX_HEIGHT = 150
MAX_WIDTH = 150

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
		layout = QVBoxLayout()

		self.tile_set_list = QComboBox()
		self.tile_set_list.addItems([tile_set.name for tile_set in self.tile_sets])
		self.tile_set_list.currentIndexChanged.connect(self.load_tile_set)
		layout.addWidget(self.tile_set_list)

		self.tile_set_name = QLineEdit()
		self.tile_set_traversal_cost = QSpinBox()
		self.tile_set_cannot_be_next_to = QLineEdit()
		self.tile_set_must_be_next_to = QLineEdit()
		self.tile_set_color = QPushButton('Select Color')
		self.tile_set_texture_path = QLineEdit()
		self.select_texture_button = QPushButton('Select Texture Path')

		layout.addWidget(QLabel('Tile Name'))
		layout.addWidget(self.tile_set_name)
		layout.addWidget(QLabel('Traversal Cost'))
		layout.addWidget(self.tile_set_traversal_cost)
		layout.addWidget(QLabel('Cannot Be Next To'))
		layout.addWidget(self.tile_set_cannot_be_next_to)
		layout.addWidget(QLabel('Must Be Next To'))
		layout.addWidget(self.tile_set_must_be_next_to)
		layout.addWidget(QLabel('Color'))
		layout.addWidget(self.tile_set_color)
		layout.addWidget(QLabel('Texture Path'))
		layout.addWidget(self.tile_set_texture_path)
		layout.addWidget(self.select_texture_button)

		self.tile_set_color.clicked.connect(self.select_color)
		self.select_texture_button.clicked.connect(self.select_texture_path)

		save_button = QPushButton('Save Changes')
		save_button.clicked.connect(self.save_changes)
		layout.addWidget(save_button)

		self.setLayout(layout)
		self.setWindowTitle('Edit Tile Sets')

	def load_tile_set(self, index):
		tile_set = self.tile_sets[index]
		self.tile_set_name.setText(tile_set.name)
		self.tile_set_traversal_cost.setValue(tile_set.traversal_cost)
		self.tile_set_cannot_be_next_to.setText(','.join(tile_set.cannot_be_next_to))
		self.tile_set_must_be_next_to.setText(','.join(tile_set.must_be_next_to))
		self.tile_set_color.setStyleSheet(f'background-color: {tile_set.color.name()}')
		self.tile_set_texture_path.setText(tile_set.texture_path)
		self.selected_color = tile_set.color

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

	def save_changes(self):
		index = self.tile_set_list.currentIndex()
		tile_set = self.tile_sets[index]
		tile_set.name = self.tile_set_name.text()
		tile_set.traversal_cost = self.tile_set_traversal_cost.value()
		tile_set.cannot_be_next_to = self.tile_set_cannot_be_next_to.text().split(',')
		tile_set.must_be_next_to = self.tile_set_must_be_next_to.text().split(',')
		tile_set.color = self.selected_color
		tile_set.texture_path = self.tile_set_texture_path.text()
		self.parent().save_tile_sets("tile_sets.json")

class MapGenerator(QWidget):
	def __init__(self):
		super().__init__()

		self.prolog = Prolog()
		#self.prolog.consult("map_rules.pl")

		self.tile_sets = []
		self.load_tile_sets("tile_sets.json")
		self.json_to_prolog("tile_sets.json", "tile_sets.pl")

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
		self.width_label = QLabel(f'Width: {MAX_WIDTH}')
		self.width_slider = QSlider(Qt.Horizontal)
		self.width_slider.setRange(5, MAX_WIDTH)
		self.width_slider.setValue(MAX_WIDTH)
		self.width_slider.valueChanged.connect(self.update_labels)
		left_layout.addWidget(self.width_label)
		left_layout.addWidget(self.width_slider)

		self.height_label = QLabel(f'Height: {MAX_HEIGHT}')
		self.height_slider = QSlider(Qt.Horizontal)
		self.height_slider.setRange(5, MAX_HEIGHT)
		self.height_slider.setValue(MAX_HEIGHT)
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

		# mock python, doesn't reflect anything
		map_data = [[None for _ in range(width)] for _ in range(height)]
		total_cells = width * height
		remaining_cells = total_cells

		for name, percentage in percentages.items():
			num_cells = (percentage * total_cells) // 100
			for _ in range(num_cells):
				while True:
					x = random.randint(0, width - 1)
					y = random.randint(0, height - 1)
					if map_data[y][x] is None:
						map_data[y][x] = name
						break
			remaining_cells -= num_cells

		# Fill remaining cells with the last tile set
		last_tile_set = list(percentages.keys())[-1]
		for y in range(height):
			for x in range(width):
				if map_data[y][x] is None:
					map_data[y][x] = last_tile_set

		self.display_map(map_data)

		# Query Prolog to generate the map (TO BE DONE)
		#query = f"generate_map({width}, {height}, {percentages}, Map)"
		#print(f"Prolog Query: {query}")
		#result = list(self.prolog.query(query))
		#if result:
		#    map_data = result[0]['Map']
		#    print("Generated Map Data:", map_data)
		#    self.display_map(map_data)
		#else:
		#    print("No result from Prolog query")
		#    self.prolog.query("print_map(Map)")

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
		for y, row in enumerate(map_data):
			for x, cell in enumerate(row):
				tile_set = next((ts for ts in self.tile_sets if ts.name == cell), None)
				if tile_set:
					color = tile_set.color
				else:
					color = QColor(0, 0, 0)  # Default to black if tile set not found
				painter.fillRect(x * cell_size, y * cell_size, cell_size, cell_size, color)
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

	def load_tile_sets(self, filename):
		try:
			with open(filename, 'r') as file:
				data = json.load(file)
				self.tile_sets = [TileSet.from_dict(item) for item in data]
		except FileNotFoundError:
			print(f"Configuration file {filename} not found. Creating default configuration.")
			self.tile_sets = self.create_default_tile_sets()
			self.save_tile_sets(filename)

	def create_default_tile_sets(self):
		default_tile_sets = [
			TileSet("water", 3, [], [], QColor(0, 0, 255), True, "", 30),
			TileSet("deep_water", 5, ["sand", "forest", "land"], ["water"], QColor(0, 0, 128), False, "", 0),
			TileSet("forest", 2, [], [], QColor(0, 128, 0), True, "", 30),
			TileSet("land", 1, [], [], QColor(255, 255, 0), True, "", 40),
			TileSet("sand", 1, [], [], QColor(252, 255, 148), False, "", 0)
		]
		return default_tile_sets


if __name__ == '__main__':
	app = QApplication(sys.argv)
	ex = MapGenerator()
	ex.show()
	sys.exit(app.exec())