import scala.math.sqrt

sealed trait Geometry {
    def x: Double;
    def y: Double;
    def z: Double;

    def distance(pt: Geometry): Double;

}

case class Point(x: Double, y: Double, z: Double = 0) extends Geometry {

    def distance(pt: Geometry): Double = {
        val dx = pt.x - x;
        val dy = pt.y - y;
        return sqrt(dx * dx + dy * dy);
    }
}

case class Point3D(x: Double, y:Double, z: Double) extends Geometry{
    def distance(pt: Geometry): Double = {
        val dx = pt.x - x;
        val dy = pt.y - y;
        val dz = pt.z - z;
        return sqrt(dx*dx + dy*dy + dz*dz);
    }
}

case class LatLong(x: Double, y: Double, z: Double = 0) extends Geometry{
    def distance(pt: Geometry): Double = {

        val d2r = Math.PI / 180

        val dLat = (pt.x - x)*d2r
        val dLon = (pt.y - y)*d2r
        val a = Math.sin(dLat/2)*Math.sin(dLat/2) + Math.cos(x*d2r)*Math.cos(pt.x*d2r)*Math.sin(dLon/2)*Math.sin(dLon/2)
        val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

        return 6384*c
    }
}