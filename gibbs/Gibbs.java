import java.util.*;
import cern.jet.random.tdouble;
import cern.jet.random.tdouble.engine.*;

public class Gibbs{
    public static void main(String[] arg){
	int N = 10000;
	int thin = 100;
	DoubleRandomEngine randEng = new DoubleMersenneTwister(new Date());
	Normal randNorm = new Normal(0.0, 1.0, randEng);
	Gamma randG = new Gamma(1.0, 1.0, randEng);
	double x = 0;
	double y = 0;
	System.out.println("Iter x y");
	for (int i=0; i < N; i++){
	    for (int i=0; i < N; i++){
		x = randG.nextDouble(3.0, y*y+1);
		    y = randNorm.nextDouble(1.0/(x+1), 1.0/Math.sqrt(2*x+2));
	    }
	    System.out.println(i+" "+x+" "+y);
	}
    }
}


