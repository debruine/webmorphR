
import Facemorph.*;
import java.lang.reflect.Method;

public class HelloWorld {
    public static void main(String[] args) {
		
		//Facemorph.Mask theMask = new Facemorph.Mask();
		Facemorph.PCI thePCI = new Facemorph.PCI();
		String theString = "string";

	    Class tClass = thePCI.getClass();
		Method[] methods = tClass.getMethods();
		for (int i = 0; i < methods.length; i++) {
			System.out.println("public method: " + methods[i]);
		}
    }
}
