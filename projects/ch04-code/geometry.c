enum ShapeType { Rectangle, Circle, Square };

struct Shape {
    enum ShapeType kind;

    union {
        struct { int width, height; } rectangleData;
        struct { int radius; } circleData;
        struct { int side;} squareData;
    } shapeData;
};


double area(struct Shape* s) {
    if( s->kind == Rectangle ) {
        int width, ht;
        width = s->shapeData.rectangleData.width;
        ht = s->shapeData.rectangleData.height;
        return width * ht;
    } else if ( s->kind == Circle ) {
        ...
