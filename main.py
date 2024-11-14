import sys
import json
import os
os.environ['SWI_HOME_DIR'] = r'C:\Program Files\swipl\bin'
from PySide6.QtWidgets import (QApplication, QWidget, QVBoxLayout, QSlider, QLabel,
                               QCheckBox, QPushButton, QGraphicsView, QGraphicsScene,
                               QGraphicsPixmapItem, QFormLayout, QLineEdit, QColorDialog,
                               QFileDialog)
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QPainter, QPixmap
from pyswip import Prolog
import random


class MapGenerator(QWidget):
    def __init__(self):
        super().__init__()

        self.tile_sets = self.load_tile_sets()  # Load tile sets from JSON
        self.pin_positions = []  # Store pinned positions

        self.initUI()
        # self.prolog = Prolog()
        # self.prolog.consult("map_rules.pl")

    def load_tile_sets(self):
        if os.path.exists("tile_sets.json"):
            with open("tile_sets.json", "r") as f:
                return json.load(f)
        return []

    def save_tile_sets(self):
        with open("tile_sets.json", "w") as f:
            json.dump(self.tile_sets, f, indent=4)

    def initUI(self):
        layout = QVBoxLayout()

        # Tile Set Editor Section
        tile_set_layout = QFormLayout()
        self.tile_name_input = QLineEdit()
        self.traversal_cost_input = QLineEdit()
        self.color_button = QPushButton("Choose Color")
        self.color_button.clicked.connect(self.choose_color)
        self.discoverable_checkbox = QCheckBox("Discoverable")
        self.texture_path_button = QPushButton("Choose Texture")
        self.texture_path_button.clicked.connect(self.choose_texture)

        tile_set_layout.addRow("Tile Name", self.tile_name_input)
        tile_set_layout.addRow("Traversal Cost", self.traversal_cost_input)
        tile_set_layout.addRow("Color", self.color_button)
        tile_set_layout.addRow("Discoverable", self.discoverable_checkbox)
        tile_set_layout.addRow("Texture Path", self.texture_path_button)

        self.save_tile_button = QPushButton("Save Tile Properties")
        self.save_tile_button.clicked.connect(self.save_tile_properties)
        tile_set_layout.addRow(self.save_tile_button)

        layout.addLayout(tile_set_layout)

        # Map Display and Pinning Section
        self.map_display = QGraphicsView()
        self.map_display.setFixedSize(500, 500)
        self.scene = QGraphicsScene(self)
        self.map_display.setScene(self.scene)
        layout.addWidget(self.map_display)
        self.map_display.mousePressEvent = self.pin_location  # Override mouse press event

        # Generate Button
        self.generate_button = QPushButton('Generate Map')
        self.generate_button.clicked.connect(self.generate_map)
        layout.addWidget(self.generate_button)

        self.setLayout(layout)
        self.setWindowTitle('Map Generator')

    def choose_color(self):
        color = QColorDialog.getColor()
        if color.isValid():
            self.color_button.setStyleSheet(f"background-color: {color.name()}")
            self.selected_color = color.name()

    def choose_texture(self):
        file_dialog = QFileDialog()
        texture_path, _ = file_dialog.getOpenFileName(self, "Choose Texture File")
        if texture_path:
            self.texture_path_button.setText(texture_path)
            self.selected_texture_path = texture_path

    def save_tile_properties(self):
        tile_name = self.tile_name_input.text()
        traversal_cost = int(self.traversal_cost_input.text())
        discoverable = self.discoverable_checkbox.isChecked()
        texture_path = getattr(self, "selected_texture_path", "")
        color = getattr(self, "selected_color", "#FFFFFF")  # Default white if not selected

        new_tile = {
            "name": tile_name,
            "traversal_cost": traversal_cost,
            "cannot_be_next_to": [],
            "must_be_next_to": [],
            "color": color,
            "discoverable": discoverable,
            "texture_path": texture_path
        }
        self.tile_sets.append(new_tile)
        self.save_tile_sets()
        print(f"Tile {tile_name} saved!")

    def generate_map(self):
        # Placeholder for map generation logic using tile properties
        width, height = 10, 10  # Example map size
        map_data = [['grass' for _ in range(width)] for _ in range(height)]
        self.display_map(map_data)

    def display_map(self, map_data):
        self.scene.clear()
        width, height = 10, 10
        cell_size = min(self.map_display.width() // width, self.map_display.height() // height)
        pixmap = QPixmap(self.map_display.width(), self.map_display.height())
        pixmap.fill(Qt.white)
        painter = QPainter(pixmap)

        for y, row in enumerate(map_data):
            for x, cell in enumerate(row):
                color = QColor(0, 255, 0)  # Default to green for grass
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

    def pin_location(self, event):
        x, y = event.pos().x(), event.pos().y()
        if len(self.pin_positions) < 2:
            self.pin_positions.append((x, y))
            print(f"Pin added at: {x}, {y}")
        if len(self.pin_positions) == 2:
            print("Both pins placed:", self.pin_positions)
            self.pin_positions.clear()  # Clear for next pinning action

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MapGenerator()
    ex.show()
    sys.exit(app.exec())
