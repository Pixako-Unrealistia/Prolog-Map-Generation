import sys
from PySide6.QtWidgets import QApplication, QWidget, QVBoxLayout, QSlider, QLabel, QHBoxLayout, QCheckBox, QPushButton, QGridLayout, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QSpinBox
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QPainter, QPixmap
from pyswip import Prolog
import random

MAX_HEIGHT = 100
MAX_WIDTH = 100

class MapGenerator(QWidget):
	def __init__(self):
		super().__init__()

		self.prolog = Prolog()
		self.prolog.consult("map_rules.pl")

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

			#temporary solution                
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
		#query = f"generate_map({width}, {height}, {water_percentage}, {forest_percentage}, {land_percentage}, {ocean_count}, {forest_count}, Map)"
		query = f"generate_map({width}, {height}, {water_percentage}, {forest_percentage}, {land_percentage}, Map)"
		print(f"Prolog Query: {query}")
		result = list(self.prolog.query(query))
		if result:
			map_data = result[0]['Map']
			print("Generated Map Data:", map_data)
			self.display_map(map_data)
		else:
			print("No result from Prolog query")
			# Additional debugging: print the map to see what was generated
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
				color = QColor(0, 255, 0)
				if cell == 'water':
					color = QColor(0, 0, 255)
				elif cell == 'forest':
					color = QColor(0, 128, 0)
				elif cell == 'sand':
					color = QColor(255, 255, 0)
				elif cell == 'ocean':
					color = QColor(0, 0, 128)
				painter.fillRect(x * cell_size, y * cell_size, cell_size, cell_size, color)
		painter.end()
		self.scene.addItem(QGraphicsPixmapItem(pixmap))

if __name__ == '__main__':
	app = QApplication(sys.argv)
	ex = MapGenerator()
	ex.show()
	sys.exit(app.exec())