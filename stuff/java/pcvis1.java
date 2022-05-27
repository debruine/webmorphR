package PsychoServlet;

/*
 * Updated 7 Jan 2015 by Lisa DeBruine
 */

import java.io.*;
import javax.imageio.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;
import javax.swing.*;
import java.awt.*;
import java.awt.image.*;
import java.util.Enumeration;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import Facemorph.*;

public class pcvis1 extends HttpServlet {
	String mydirectory = null;
    String debugFolder = null;
      
			      
    public void init() throws ServletException {
        try {
        	debugFolder = this.getInitParameter("debugFolder");
			PrintStream fos = new PrintStream(debugFolder + "pcvis1_init.txt");
			fos.println("Initialising pc reconstruction servlet v.1");
			mydirectory = this.getInitParameter("mydirectory");
			fos.println("mydirectory = " + mydirectory);

	        fos.println("Completed initialising pc reconstruction servlet v.1");
	        fos.flush();
	        fos.close();
        } catch (IOException e) {
            System.out.println(e);
            throw new ServletException(e);
        }
    }
   
	 
    public void doPost (HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
        doGet(req,res);
    }
    
	
     public void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
    	Date date = new Date();
    	
    	// create error data for returning error info via json
    	String hasError = "false";
    	String errorText = "";
    
        PrintStream fos = new PrintStream(debugFolder + "pcvis1_debug.txt");
        fos.println("Starting PC Reconstruction (" + date.toString() + ")");
        
       	// subfolder for image saving ( should be {ID} )
		String subfolder = "";
		if (req.getParameter("subfolder") != null) {
			subfolder = req.getParameter("subfolder");
		}
		
		// optional savefolder 
		String savefolder = "/.tmp/";
		if (req.getParameter("savefolder") != null) {
			savefolder = req.getParameter("savefolder");
		}

		fos.println("Using output folder: " + mydirectory + subfolder + savefolder);
		File uploadDir = new File(mydirectory + subfolder + savefolder);

		/*****************************************
		 get PCA model and average to transform
		 *****************************************/	
		
		Facemorph.PCA myPCA = new Facemorph.PCA();
		Facemorph.Template avgTem = new Facemorph.Template();
		Image avgImg = null;
	        
        if (req.getParameter("pcafile") != null) {
			String pcafilename = mydirectory + subfolder + req.getParameter("pcafile");
			fos.println("Reading PCA model: " + pcafilename);
			myPCA.readBinary(pcafilename, 100);
		} else {
			hasError = "true";
			errorText = "No pca file specified";
			fos.println(errorText);
		}
		
		if (req.getParameter("avgfile") != null) {	
			String avgImgName = mydirectory + subfolder + req.getParameter("avgfile");
			fos.println("Reading average image: " + avgImgName);
			ImageIcon ii = new ImageIcon(avgImgName);
			avgImg = ii.getImage();
			
			String cleanName = req.getParameter("avgfile").replace(".jpg", "").replace(".png", "").replace(".gif", "");
			String avgTemName = mydirectory + subfolder + cleanName + ".tem";
			fos.println("Reading average template: " + avgTemName);
			avgTem.read(avgTemName);
			
		} else {
			hasError = "true";
			errorText = "No average file specified";
			fos.println(errorText);
		}
		
		/*****************************************
		 reconstruct template
		 *****************************************/
		
		int c = myPCA.getCount();

		// create a template from an array of PC weights
		String[] pc_weights = { "100" };
		if (req.getParameter("pc_weights") != null) {
			pc_weights = req.getParameter("pc_weights").split(",");
		}
		
		int pc_array_length = pc_weights.length;
		if (pc_array_length > c) { pc_array_length = c; }  // make sure to only read as many PCs as are available
		
		float[] pc_weights_array = new float[pc_array_length];
		String pc_check = "";
		for (int j = 0; j < pc_array_length; j++) {
			pc_weights_array[j] = Float.parseFloat(pc_weights[j].trim());
			pc_check = pc_check + "," + pc_weights_array[j];
		}
		fos.println("Reconstructing [" + pc_check.substring(1) + "]");

		
		Facemorph.ASM myASM = new ASM(avgTem, myPCA, avgImg, ASM.RIGID_BODY_NORMALISATION, null);
		Facemorph.Template pcTem = new Facemorph.Template();
		Facemorph.Template newTem = new Facemorph.Template();
        Image newImg = null;
        
        // create reconstructed template from PC weights
        pcTem = myASM.reconstruct(pc_weights_array);

        // warp average image into new tem shape
        // avgImg is destination image because no shape or texture transformation is being done
	    newImg = Transformer.testTransform(Warp.MULTISCALE, avgTem, avgTem, pcTem, newTem, 
											avgImg, avgImg, avgImg, 
											1.0, 0, 0, 
											null, 1, null, true, true,
											false, ASM.RIGID_BODY_NORMALISATION, null);
		
		String jsonOutput = "";								
		if (newImg==null)  {
			hasError = "true";
        	errorText = "The image was not made";
        	fos.println(errorText);
        	
        	jsonOutput = String.format(	
				"{ " + "\n" +
					"\"error\": %s, " + "\n" +
					"\"errorText\": \"%s\"," + "\n" +
					"\"pcafile\": \"%s\"," + "\n" +
					"\"avgfile\": \"%s\"," + "\n" +
					"\"weights\": [%s]," + "\n" +
					"\"savefolder\": \"%s\"" + "\n" +
				" }",
				hasError, 
				errorText,
				req.getParameter("pcafile"),
				req.getParameter("avgfile"),
				pc_check.substring(1),
				savefolder
			);
        } else {
	        String myFormat = req.getParameter("format");
			fos.println("Format = " + myFormat);
            if (myFormat != "jpg" && myFormat != "png" && myFormat != "gif") {
            	myFormat = "jpg";
            }
            
            File f = File.createTempFile("avg", "." + myFormat, uploadDir);
            BufferedImage bi = Transformer.ImageToBufferedImage(newImg, null); 
            ImageIO.write(bi, myFormat, f);
            int width              = bi.getWidth();
			int height             = bi.getHeight();
		
			// write out .tem file
			String tpath = f.getName().replace("." + myFormat, ".tem");
			File t = new File(uploadDir, tpath);
			t.createNewFile();
			FileOutputStream temout = new FileOutputStream(t);
			PrintStream tem_ps = new PrintStream(new BufferedOutputStream(temout), true);
			newTem.write(tem_ps);
			tem_ps.flush();
			tem_ps.close();
			
			jsonOutput = String.format(	
				"{ " + "\n" +
					"\"error\": %s, " + "\n" +
					"\"errorText\": \"%s\"," + "\n" +
					"\"pcafile\": \"%s\"," + "\n" +
					"\"avgfile\": \"%s\"," + "\n" +
					"\"weights\": [%s]," + "\n" +
					"\"savefolder\": \"%s\"," + "\n" +
					"\"img\": \"%s\"," + "\n" +
					"\"tem\": \"%s\"" + "\n" +
				" }",
				hasError, 
				errorText,
				req.getParameter("pcafile"),
				req.getParameter("avgfile"),
				pc_check.substring(1),
				savefolder,
				f.getName(),
				t.getName()
				
			);
        }
											
		/*****************************************
		 Return error data
		 *****************************************/		
		
		// return analysis
        fos.println(jsonOutput);

        //New print writer to write output to		
		res.setContentType("application/json");
		PrintWriter out = res.getWriter();
		out.println(jsonOutput);
		
        fos.flush();
        fos.close();
   
        out.flush();
        out.close();
    } 
}