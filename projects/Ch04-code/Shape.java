abstract class Shape {
    abstract double area();
}

class Circle extends Shape {
    final double radius;
    Circle(double radius) { this.radius = radius; }
    double area() { return Math.PI * radius*radius; }
}

class Rectangle extends Shape {
    final double ht;
    final double width;
    Rectangle(double width, double height) {
        this.ht = height;
        this.width = width;
    }
    double area() { return width * ht; }
}

class Square extends Shape {
    final double side;
    Square(double side) {
        this.side = side;
    }
    double area() { return side * side; }
}
