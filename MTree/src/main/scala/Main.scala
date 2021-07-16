import java.io.{File, PrintWriter}

object Main extends App{
    val writer = new PrintWriter(new File("benchmark_2D.txt"));
    for (k <- Seq(10,50,100)) {
        var mt = new MTree[String](M = k);

        var bufferedSource = io.Source.fromFile("points_small.dat");
        var i = 1;
        var duration = 0.0;
        var v = Vector[Entry[String]]();
        var bPoints = Vector[Point]();
        for (line <- bufferedSource.getLines) {
            val cols = line.split(",");

            val pt = new Point(cols(1).toDouble, cols(2).toDouble);
            if (bPoints.length < 5000) bPoints = bPoints :+ pt;
            val e = new Entry[String](pt, 0, cols(0));
            if (v.length < 5000) v = v :+ e;
            var t1 = System.nanoTime;
            mt.insert(e);
            duration = duration + (System.nanoTime - t1) / 1e9d;

            i += 1
        }
        println("---------Test for M = " + k  + "------------");
        println("Insertion time of " + (i-1) + " entries: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");


        duration = 0;
        for (c <- bPoints) {
            var t2 = System.nanoTime;
            var search = mt.RangeSearch(c, 1000);
            duration = duration + (System.nanoTime - t2) / 1e9d;
        }
        println("RangeSearch time of "+ bPoints.length +" points (search radius = 300): " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");
/*        println("Results:");
        for (el <- search){
            println(el.value + ": " + el.center);
        }*/

        duration = 0;
        var neighbors = 10;
        for (c <- bPoints) {
            var t3 = System.nanoTime;
            var arr = mt.kNNSearch(c, neighbors);
            duration = duration + (System.nanoTime - t3) / 1e9d;
        }
        println(neighbors + "-NN time of "+ bPoints.length +" points: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");
/*        println("Results:");
        for (el <- arr){
            println(el._1.value);
        }*/

        duration = 0;
        var l = 0;
        for (e <- v){
            var t5 = System.nanoTime();
           if (!mt.remove(e)) {
               l = l + 1;
           }
            duration = duration + (System.nanoTime - t5) / 1e9d;
        }
        println("Removal errors: " + l);
        println("Removal time of " + v.length + " entries: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");

        writer.write("\n");
    }


    for (k <- Seq(10,50,100)) {
        var mt = new MTree[String](M = k);

        var bufferedSource = io.Source.fromFile("points_medium.dat");
        var i = 1;
        var duration = 0.0;
        var v = Vector[Entry[String]]();
        var bPoints = Vector[Point]();
        for (line <- bufferedSource.getLines) {
            val cols = line.split(",");

            val pt = new Point(cols(1).toDouble, cols(2).toDouble);
            if (bPoints.length < 5000) bPoints = bPoints :+ pt;
            val e = new Entry[String](pt, 0, cols(0));
            if (v.length < 5000) v = v :+ e;
            var t1 = System.nanoTime;
            mt.insert(e);
            duration = duration + (System.nanoTime - t1) / 1e9d;

            i += 1
        }
        println("---------Test for M = " + k  + "------------");
        println("Insertion time of " + (i-1) + " entries: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");


        duration = 0;
        for (c <- bPoints) {
            var t2 = System.nanoTime;
            var search = mt.RangeSearch(c, 300);
            duration = duration + (System.nanoTime - t2) / 1e9d;
        }
        println("RangeSearch time of "+ bPoints.length +" points (search radius = 300): " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");
        /*        println("Results:");
                for (el <- search){
                    println(el.value + ": " + el.center);
                }*/

        duration = 0;
        var neighbors = 10;
        for (c <- bPoints) {
            var t3 = System.nanoTime;
            var arr = mt.kNNSearch(c, neighbors);
            duration = duration + (System.nanoTime - t3) / 1e9d;
        }
        println(neighbors + "-NN time of "+ bPoints.length +" points: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");
/*                println("Results:");
                for (el <- arr){
                    println(el._1.value);
                }*/

        duration = 0;
        var l = 0;
        for (e <- v){
            var t5 = System.nanoTime();
            if (!mt.remove(e)) {
                l = l + 1;
            }
            duration = duration + (System.nanoTime - t5) / 1e9d;
        }
        println("Removal errors: " + l);
        println("Removal time of " + v.length + " entries: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");

        writer.write("\n");
    }

    for (k <- Seq(10,50,100)) {
        var mt = new MTree[String](M = k);

        var bufferedSource = io.Source.fromFile("points_large.dat");
        var i = 1;
        var duration = 0.0;
        var v = Vector[Entry[String]]();
        var bPoints = Vector[Point]();
        for (line <- bufferedSource.getLines) {
            val cols = line.split(",");

            val pt = new Point(cols(1).toDouble, cols(2).toDouble);
            if (bPoints.length < 5000) bPoints = bPoints :+ pt;
            val e = new Entry[String](pt, 0, cols(0));
            if (v.length < 5000) v = v :+ e;
            var t1 = System.nanoTime;
            mt.insert(e);
            duration = duration + (System.nanoTime - t1) / 1e9d;

            i += 1
        }
        println("---------Test for M = " + k  + "------------");
        println("Insertion time of " + (i-1) + " entries: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");


        duration = 0;
        for (c <- bPoints) {
            var t2 = System.nanoTime;
            var search = mt.RangeSearch(c, 300);
            duration = duration + (System.nanoTime - t2) / 1e9d;
        }
        println("RangeSearch time of "+ bPoints.length +" points (search radius = 300): " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");
        /*        println("Results:");
                for (el <- search){
                    println(el.value + ": " + el.center);
                }*/

        duration = 0;
        var neighbors = 10;
        for (c <- bPoints) {
            var t3 = System.nanoTime;
            var arr = mt.kNNSearch(c, neighbors);
            duration = duration + (System.nanoTime - t3) / 1e9d;
        }
        println(neighbors + "-NN time of "+ bPoints.length +" points: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");
/*                println("Results:");
                for (el <- arr){
                    println(el._1.value);
                }*/

        duration = 0;
        var l = 0;
        for (e <- v){
            var t5 = System.nanoTime();
            if (!mt.remove(e)) {
                l = l + 1;
            }
            duration = duration + (System.nanoTime - t5) / 1e9d;
        }
        println("Removal errors: " + l);
        println("Removal time of " + v.length + " entries: " + duration + " seconds\n");
        writer.write(String.valueOf(duration)+"\n");

        writer.write("\n");
    }
    writer.close();

}
