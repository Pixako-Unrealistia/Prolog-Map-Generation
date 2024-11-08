import sys
from PySide6.QtWidgets import QApplication, QWidget, QVBoxLayout, QSlider, QLabel, QHBoxLayout, QCheckBox, QPushButton, QGridLayout, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QSpinBox, QLineEdit, QColorDialog, QFileDialog, QComboBox
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QPainter, QPixmap
from pyswip import Prolog
import random
import json
MAX_HEIGHT = 100
MAX_WIDTH = 100

class TileSet:
	def __init__(self, name, traversal_cost, cannot_be_next_to, must_be_next_to, color, texture_path):
		self.name = name
		self.traversal_cost = traversal_cost
		self.cannot_be_next_to = cannot_be_next_to
		self.must_be_next_to = must_be_next_to
		self.color = color
		self.texture_path = texture_path

	def to_dict(self):
		return {
			'name': self.name,
			'traversal_cost': self.traversal_cost,
			'cannot_be_next_to': self.cannot_be_next_to,
			'must_be_next_to': self.must_be_next_to,
			'color': self.color.name(),
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
			data['texture_path']
		)

class TileSetEditor(QWidget):
	def __init__(self, tile_sets, parent=None):
		super().__init__(parent)
		self.tile_sets = tile_sets
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

		self.initUI()

	def initUI(self):
		layout = QVBoxLayout()

		# Water Percentage Slider
		self.water_label = QLabel('Water Percentage: 30%')
		self.water_slider = QSlider(Qt.Horizontal)
		self.water_slider.setRange(0, 100)
		self.water_slider.setValue(30)  # Default value
		self.water_slider.valueChanged.connect(self.update_labels)
		layout.addWidget(self.water_label)
		layout.addWidget(self.water_slider)

		# Forest Percentage Slider
		self.forest_label = QLabel('Forest Percentage: 30%')
		self.forest_slider = QSlider(Qt.Horizontal)
		self.forest_slider.setRange(0, 100)
		self.forest_slider.setValue(30)  # Default value
		self.forest_slider.valueChanged.connect(self.update_labels)
		layout.addWidget(self.forest_label)
		layout.addWidget(self.forest_slider)

		# Land Percentage Slider
		self.land_label = QLabel('Land Percentage: 40%')
		self.land_slider = QSlider(Qt.Horizontal)
		self.land_slider.setRange(0, 100)
		self.land_slider.setValue(40)  # Default value
		self.land_slider.valueChanged.connect(self.update_labels)
		layout.addWidget(self.land_label)
		layout.addWidget(self.land_slider)

		# Seed Label and Button
		self.seed_label = QLabel('Seed: 0')
		self.regenerate_button = QPushButton('Regenerate Seed')
		self.regenerate_button.clicked.connect(self.regenerate_seed)
		layout.addWidget(self.seed_label)
		layout.addWidget(self.regenerate_button)

		# Width and Height Sliders
		self.width_label = QLabel(f'Width: {MAX_WIDTH}')
		self.width_slider = QSlider(Qt.Horizontal)
		self.width_slider.setRange(5, MAX_WIDTH)
		self.width_slider.setValue(MAX_WIDTH)
		self.width_slider.valueChanged.connect(self.update_labels)
		layout.addWidget(self.width_label)
		layout.addWidget(self.width_slider)

		self.height_label = QLabel(f'Height: {MAX_HEIGHT}')
		self.height_slider = QSlider(Qt.Horizontal)
		self.height_slider.setRange(5, MAX_HEIGHT)
		self.height_slider.setValue(MAX_HEIGHT)
		self.height_slider.valueChanged.connect(self.update_labels)
		layout.addWidget(self.height_label)
		layout.addWidget(self.height_slider)

		# Ocean Count SpinBox
		self.ocean_count_label = QLabel('Ocean Count: 1')
		self.ocean_count_spinbox = QSpinBox()
		self.ocean_count_spinbox.setRange(0, 100)
		self.ocean_count_spinbox.setValue(1)  # Default value
		self.ocean_count_spinbox.valueChanged.connect(self.update_labels)
		layout.addWidget(self.ocean_count_label)
		layout.addWidget(self.ocean_count_spinbox)

		# Forest Count SpinBox
		self.forest_count_label = QLabel('Forest Count: 1')
		self.forest_count_spinbox = QSpinBox()
		self.forest_count_spinbox.setRange(0, 100)
		self.forest_count_spinbox.setValue(1)  # Default value
		self.forest_count_spinbox.valueChanged.connect(self.update_labels)
		layout.addWidget(self.forest_count_label)
		layout.addWidget(self.forest_count_spinbox)

		# Auto Adjust Checkbox
		self.auto_adjust_checkbox = QCheckBox('Auto Adjust Sliders')
		layout.addWidget(self.auto_adjust_checkbox)

		# Generate Button
		self.generate_button = QPushButton('Generate Map')
		self.generate_button.clicked.connect(self.generate_map)
		layout.addWidget(self.generate_button)

		# Map Display
		self.map_display = QGraphicsView()
		self.map_display.setFixedSize(500, 500)
		self.scene = QGraphicsScene(self)
		self.map_display.setScene(self.scene)
		layout.addWidget(self.map_display)

		# Edit Tile Sets Button
		edit_button = QPushButton('Edit Tile Sets')
		edit_button.clicked.connect(self.open_tile_set_editor)
		layout.addWidget(edit_button)

		self.setLayout(layout)
		self.setWindowTitle('Map Generator')

	def update_labels(self):
		water = self.water_slider.value()
		forest = self.forest_slider.value()
		land = self.land_slider.value()
		width = self.width_slider.value()
		height = self.height_slider.value()

		total = water + forest + land
		if total > 100 and self.auto_adjust_checkbox.isChecked():
			excess = total - 100
			if self.sender() == self.water_slider:
				self.water_slider.setValue(water - excess)
			elif self.sender() == self.forest_slider:
				self.forest_slider.setValue(forest - excess)
			elif self.sender() == self.land_slider:
				self.land_slider.setValue(land - excess)

		elif not self.auto_adjust_checkbox.isChecked():
			remaining = 100 - total
			if self.sender() == self.water_slider:
				self.forest_slider.setValue(max(0, forest + remaining // 2))
				self.land_slider.setValue(max(0, land + remaining // 2 + remaining % 2))
			elif self.sender() == self.forest_slider:
				self.water_slider.setValue(max(0, water + remaining // 2))
				self.land_slider.setValue(max(0, land + remaining // 2 + remaining % 2))
			elif self.sender() == self.land_slider:
				self.water_slider.setValue(max(0, water + remaining // 2))
				self.forest_slider.setValue(max(0, forest + remaining // 2 + remaining % 2))

			if self.water_slider.value() == 100:
				self.forest_slider.setValue(0)
				self.land_slider.setValue(0)
			elif self.forest_slider.value() == 100:
				self.water_slider.setValue(0)
				self.land_slider.setValue(0)
			elif self.land_slider.value() == 100:
				self.water_slider.setValue(0)
				self.forest_slider.setValue(0)

			# Temporary solution
			total = self.water_slider.value() + self.forest_slider.value() + self.land_slider.value()
			if total > 100:
				if self.water_slider.value() < self.forest_slider.value() and self.water_slider.value() < self.land_slider.value():
					self.water_slider.setValue(self.water_slider.value() - 1)
				elif self.forest_slider.value() < self.water_slider.value() and self.forest_slider.value() < self.land_slider.value():
					self.forest_slider.setValue(self.forest_slider.value() - 1)
				else:
					self.land_slider.setValue(self.land_slider.value() - 1)

		self.water_label.setText(f'Water Percentage: {self.water_slider.value()}%')
		self.forest_label.setText(f'Forest Percentage: {self.forest_slider.value()}%')
		self.land_label.setText(f'Land Percentage: {self.land_slider.value()}%')
		self.width_label.setText(f'Width: {self.width_slider.value()}')
		self.height_label.setText(f'Height: {self.height_slider.value()}')
		self.ocean_count_label.setText(f'Ocean Count: {self.ocean_count_spinbox.value()}')
		self.forest_count_label.setText(f'Forest Count: {self.forest_count_spinbox.value()}')

	def regenerate_seed(self):
		result = list(self.prolog.query("random_seed(Seed)"))
		if result:
			seed = result[0]['Seed']
			self.seed_label.setText(f'Seed: {seed}')

	def generate_map(self):
		width = self.width_slider.value()
		height = self.height_slider.value()
		water_percentage = self.water_slider.value()
		forest_percentage = self.forest_slider.value()
		land_percentage = self.land_slider.value()
		forest_count = self.forest_count_spinbox.value()
		ocean_count = self.ocean_count_spinbox.value()

		if water_percentage + forest_percentage + land_percentage != 100:
			raise ValueError("The sum of water, forest, and land percentages must be 100.")

		# Query Prolog to generate the map
		query = f"generate_map({width}, {height}, {water_percentage}, {forest_percentage}, {land_percentage}, Map)"
		print(f"Prolog Query: {query}")
		result = list(self.prolog.query(query))
		if result:
			map_data = result[0]['Map']
			print("Generated Map Data:", map_data)
			self.display_map(map_data)
		else:
			print("No result from Prolog query")
			self.prolog.query("print_map(Map)")

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
		self.editor.show()

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
			json.dump([tile_set.to_dict() for tile_set in self.tile_sets], file)

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
			TileSet("water", 1, [], [], QColor(0, 0, 255), ""),
			TileSet("forest", 2, [], [], QColor(0, 128, 0), ""),
			TileSet("land", 1, [], [], QColor(255, 255, 0), "")
		]
		return default_tile_sets

if __name__ == '__main__':
	app = QApplication(sys.argv)
	ex = MapGenerator()
	ex.show()
	sys.exit(app.exec())