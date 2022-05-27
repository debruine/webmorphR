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


public class pca1 extends HttpServlet {
	String mydirectory = null;
    String debugFolder = null;
    
    // create default filters
	float[] hvals = {0.0625f, 0.25f, 0.375f, 0.25f, 0.0625f};
	Facemorph.Filter H1 = new Facemorph.Filter(2, hvals);
	float[] gvals = {0.0625f, -0.25f, 0.375f, -0.25f, 0.0625f};
	Facemorph.Filter G = new Facemorph.Filter(2, gvals);
	float[] l1vals = {0.0625f, 0.5f, 1.375f, 0.5f, 0.0625f};
	Facemorph.Filter L1 = new Facemorph.Filter(2, l1vals);
	float[] l2vals = {0.0625f, 0.5f, 1.6875f, 3.0f, 1.6875f, 0.5f, 0.0625f};
	Facemorph.Filter L2 = new Facemorph.Filter(3, l2vals);
	float[] k1vals = {-0.0078125f, -0.03125f, 0.03125f, 0.28125f, 1.453125f, 0.28125f, 0.03125f, -0.03125f, -0.0078125f};
	Facemorph.Filter K1 = new Facemorph.Filter(4, k1vals);
	float[] k2vals = {-0.0078125f, -0.03125f, -0.0078125f, 0.125f, 0.265625f, 1.3125f, 0.265625f, 0.125f, -0.0078125f, -0.03125f, -0.0078125f};
	Facemorph.Filter K2 = new Facemorph.Filter(5, k2vals);
	float[] h2vals = {-0.125f, -0.125f, 0.625f, 1.25f, 0.625f, -0.125f, -0.125f};
	Facemorph.Filter H2 = new Facemorph.Filter(3, h2vals);
	Facemorph.Filter[] faceFilter = {H1, G, H2, L1, K1, L2, K2};
			      
    public void init() throws ServletException {
        try {
        	debugFolder = this.getInitParameter("debugFolder");
			PrintStream fos = new PrintStream(debugFolder + "pca1_init.txt");
			fos.println("Initialising pca servlet v.1");
			mydirectory = this.getInitParameter("mydirectory");
			fos.println("    mydirectory = " + mydirectory);
			fos.println("Completed initialising pca servlet v.1");
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
    	File f = null; // generic file object for checking file existence
    	Path p = null; // generic path object for checking file existence
    	
    	// create error data for returning error info via json
    	String hasError = "false";
    	String errorText = "";
    	ArrayList<String> newFiles = new ArrayList<String>();
    
        PrintStream fos = new PrintStream(debugFolder + "pca1_debug.txt");
        fos.println("Starting PCA (" + date.toString() + ")");

		/***********************************************************
		req data:
			subfolder		String	user ID for locating user folder
			pca:			Boolean	do any shape PCA functions
			usepca: 		Boolean	load PCA model from pcafile
			pcafile: 		String	name of PCA model to load or save
			analysepca: 	Boolean	analyse shape of images
			sanalysisfile: 	String	name of shape analysis file
			pci:			Boolean	do any color PCI functions
			usepci: 		Boolean load PCI model from pcifile
			pcifile: 		String	name of PCI model to load or save
			analysepci: 	Boolean	analyse colour of images
			canalysisfile: 	String	name of colour analysis file
			images: 		String[] list of image paths
			texture: 		Boolean	use texture to create average
		***********************************************************/
		         
       	// subfolder for image saving ( should be {ID} )
		String subfolder = "";
		if (req.getParameter("subfolder") != null) {
			subfolder = req.getParameter("subfolder");
		}
		
		// check if PCA/PCI functions are needed
        Boolean doPCA = false;
        if (req.getParameter("pca") != null) {
        	doPCA = Boolean.parseBoolean(req.getParameter("pca"));
        }
        Boolean doPCI = false;
        if (req.getParameter("pci") != null) {
        	doPCI = Boolean.parseBoolean(req.getParameter("pci"));
        }
									
		// check if the PCA/PCI needs to be created and saved
        Boolean usePCA = false;
        if (req.getParameter("usepca") != null) {
        	usePCA = Boolean.parseBoolean(req.getParameter("usepca"));
        }
        Boolean usePCI = false;
        if (req.getParameter("usepci") != null) {
        	usePCI = Boolean.parseBoolean(req.getParameter("usepci"));
        }
        
        // get model names, defaults to _model
		String pcafile = "_model";
		if (req.getParameter("pcafile") != null) {
			pcafile = req.getParameter("pcafile");
		}
		String pcifile = "_model";
		if (req.getParameter("pcifile") != null) {
			pcifile = req.getParameter("pcifile");
		}
		
		// check if PCA/PCI analyses should be done
		Boolean analysePCA = false;
		if (req.getParameter("analysepca") != null) {
			analysePCA = Boolean.parseBoolean(req.getParameter("analysepca"));
		}
		Boolean analysePCI = false;
		if (req.getParameter("analysepci") != null) {
			analysePCI = Boolean.parseBoolean(req.getParameter("analysepci"));
		}
		
		// get analysis file names, defaults to _analysis
		String sanalysisfile = "_analysis";
		if (req.getParameter("sanalysisfile") != null) {
			sanalysisfile = req.getParameter("sanalysisfile");
		}
		String canalysisfile = "_analysis";
		if (req.getParameter("canalysisfile") != null) {
			canalysisfile = req.getParameter("canalysisfile");
		}
		
		boolean texture = false;
        if (req.getParameter("texture") != null) {     
        	texture = Boolean.parseBoolean(req.getParameter("texture"));
        }
		
        
		/*****************************************
		 Load Images and tems
		 *****************************************/        
        
		String[] theImages = null;
        if (req.getParameterValues("images") != null) {
        	theImages = req.getParameterValues("images");
        }
        int imgN = theImages.length;
        
        Image[] imlist = new Image[imgN]; // image list for averaging
        Facemorph.Template[] tmlist = new Facemorph.Template[imgN]; // template list for averaging
        ArrayList<Template> temlist = new ArrayList<Template>(imgN); // template list for PCA
        String[] imgNames = new String[imgN]; // template list for names
        
        int width = 0;
        int height = 0;
        
        for (int j = 0; j<imgN; j++) {
			// make sure there is no extension on the name
        	String cleanName = theImages[j].replace(".jpg", "")
        							   .replace(".png", "")
        							   .replace(".gif", "")
        							   .replace(".tem", "");
		
			// load img and tem
			date = new Date();
			fos.println("Loading " + theImages[j] + " (" + date.toString() + ")");
			
			String imgName = mydirectory + subfolder + theImages[j];
			String temName = mydirectory + subfolder + cleanName + ".tem";

			//fos.println("    Reading template from " + temName);
			
			File t = new File(temName);
			if(!t.exists()) {
				fos.println("  " + temName + " does not exist");
				hasError = "true";
				errorText = temName + " does not exist";
				break;
			}
			
			Facemorph.Template tem = new Facemorph.Template();
			tem.read(temName);

			//fos.println("    Reading image from " + imgName);

			t = new File(imgName);
			if(t.exists()) {
				ImageIcon ii = new ImageIcon(imgName);
				if (j == 0) {
					height = ii.getIconHeight();
					width = ii.getIconWidth();
					fos.println("Width = " + width + ", Height = " + height);
				}
				imlist[j] = ii.getImage();
				tmlist[j] = tem;
				imgNames[j] = cleanName;
				temlist.add(tem);
			} else {
				fos.println("  " + imgName + " does not exist");
			}
		}
		
		// set up average image and tem 
		Facemorph.Template avgTem = new Facemorph.Template();
		Image avgImg = null;
		String imgfilepath = "";
		String temfilepath = "";
		
		/*****************************************
		 PCA
		 *****************************************/
		if (doPCA) {
	    	Facemorph.PCA myPCA = new Facemorph.PCA();
			Facemorph.Template pcaTemplate = new Facemorph.Template();
	        
	        if (usePCA) {
				fos.println("Loading Saved PCA");
	
				String pcaToLoad = mydirectory + subfolder + pcafile + ".pca";
				fos.println("--Reading PCA model: " + pcaToLoad);
				f = new File(pcaToLoad);
				
				if (f.exists() && !f.isDirectory()) {
					myPCA.readBinary(pcaToLoad, 100);
					
					String modelTemName = pcaToLoad.replace(".pca", ".tem");
					fos.println("--Reading model template: " + modelTemName);
					avgTem.read(modelTemName);
					
					String modelImgName = pcaToLoad.replace(".pca", ".jpg");
					fos.println("--Reading model image: " + modelImgName);
					ImageIcon ii = new ImageIcon(modelImgName);
					avgImg = ii.getImage();
				} else {
					fos.println("--PCA model could not load");
					hasError = "true";
					errorText = "PCA model could not load";
				}
			} else { // create and save a new PCA
		        fos.println("Starting PCA Analysis");
		        fos.println("Template list length = " + temlist.size());
		        
			    pcaTemplate = myPCA.build(temlist, ASM.RIGID_BODY_NORMALISATION, null);
	
		        if (pcaTemplate==null) {
		        	 fos.println("  PCA analysis failed");
		        	 hasError = "true";
		        	 errorText = "PCA analysis failed";
		        } else {
			        fos.println("PCA success");
			        
			        // write out average tem
					writeTem(pcaTemplate, mydirectory + subfolder + pcafile + ".tem");
					fos.println("--Created: " + pcafile + ".tem");
			        
			        // write out .pca file in binary and text formats
					String pcafilepath = mydirectory + subfolder + pcafile + ".pca";
					p = Paths.get(pcafilepath);
					if (Files.deleteIfExists(p)) { fos.println("--Deleted " + pcafilepath); }
					myPCA.writeBinary(pcafilepath);
					fos.println("--Created: " + pcafile + ".pca");
					//newFiles.add(pcafilepath.replace(mydirectory + subfolder));
					
					String pcatxtfilepath = mydirectory + subfolder + pcafile + ".pca.txt";
					p = Paths.get(pcatxtfilepath);
					if (Files.deleteIfExists(p)) { fos.println("--Deleted " + pcatxtfilepath); }
					myPCA.writeText(pcatxtfilepath);
					fos.println("--Created: " + pcafile + ".pca.txt");
					//newFiles.add(pcatxtfilepath.replace(mydirectory + subfolder));
					
					// write out variance table
					int c = myPCA.getCount();
					double totalVars = 0.0;
					for (int i=0; i<c; i++) {
						totalVars = totalVars + myPCA.getD(i);
					}
					String myVars = "PC,variance,percent,cumulative\n";
					double cumVars = 0.0;
					for (int i=0; i<c; i++) {
						double v = myPCA.getD(i);
						double pVar = (myPCA.getD(i) / totalVars);
						cumVars = cumVars + pVar;
						
						myVars = myVars + "PC" + i + "," + ((double)Math.round(v * 10) / 10) +"," 
								+ ((double)Math.round(pVar * 100 * 1000) / 1000) + "," 
								+ ((double)Math.round(cumVars * 100 * 1000) / 1000) + "\n";
					}
		
					String varfilepath = mydirectory + subfolder + pcafile + ".pca.vars.csv";
					p = Paths.get(varfilepath);
					if (Files.deleteIfExists(p)) { fos.println("--Deleted " + varfilepath); }
					writeTxt(myVars, varfilepath);
					fos.println("--Created: " + pcafile + ".pca.vars.csv");
					//newFiles.add(varfilepath.replace(mydirectory + subfolder));
					
					// create average image
					Image preAvgImg = null;
					Facemorph.Template preAvgTem = new Facemorph.Template();
					
					if (texture) {
		            	preAvgImg = Transformer.averageImagesTexture(imlist, tmlist, preAvgTem, width, height, faceFilter, 1, null, false, ASM.RIGID_BODY_NORMALISATION);
		            } else {
		            	preAvgImg = Transformer.averageImages(imlist, tmlist, preAvgTem, width, height, null, false, ASM.RIGID_BODY_NORMALISATION);
		            }
		            
		            // warp the average into the exact PCA template
		            avgImg = Transformer.testTransform(Warp.MULTISCALE, preAvgTem, preAvgTem, pcaTemplate, avgTem, 
													preAvgImg, preAvgImg, preAvgImg, 
													1.0, 0.0, 0.0, 
													faceFilter, 1, null, true, true,
													false, ASM.NO_NORMALISATION, null);
	
		            //write out average image (average tem is saved right after PCA is built
		            File a = new File(mydirectory + subfolder + pcafile + ".jpg");
					a.createNewFile();
		            BufferedImage bi = Transformer.ImageToBufferedImage(avgImg, null); 
					ImageIO.write(bi, "jpg", a);
					fos.println("--Created: " + pcafile + ".jpg");
				}
			}
			
			// analyse all templates if analyse variable is true
			if (analysePCA) {
		    	String myAnalysis = "Image";
		    	
		    	// make header
		    	int pcn = myPCA.getCount();
		    	for (int i=0; i<pcn; i++) {
		    		myAnalysis = myAnalysis + ",PC" + i;
		    	}
		    	myAnalysis = myAnalysis + "\n";
		    	
			    for (int i=0; i<tmlist.length; i++) {
				    double[] weights = myPCA.analyse(tmlist[i], avgTem, ASM.RIGID_BODY_NORMALISATION, null);
				    
				    myAnalysis = myAnalysis + imgNames[i] + ",";
				    for (int j=0; j<weights.length-1; j++) {
				    	myAnalysis = myAnalysis + weights[j] + ",";
				    }
				    myAnalysis = myAnalysis + weights[weights.length-1] + "\n";
				}
				
				String analysisfilepath = mydirectory + subfolder + sanalysisfile + ".shape.csv";
				writeTxt(myAnalysis, analysisfilepath);
				fos.println("--Created: " + analysisfilepath);
				
			}
		}
			
		/*****************************************
		 PCI 
		 *****************************************/
		
		if (doPCI) {
			Facemorph.PCI myPCI = new Facemorph.PCI();
			Facemorph.Mask pciMask = new Facemorph.Mask();
			
			String mask = "frl_face";
			if (req.getParameter("mask") != null) {
				mask = req.getParameter("mask");
			}
			
			int[][] pl_no_ears 			= { {29, 0}, {25, 0}, {28, 1}, {24, 1} };
			int[][] pl_with_ears 		= { {29, 0}, {27, 0}, {28, 1}, {26, 1} };
			int[][] frl_face 			= { {42, 0}, {43, 0}, {25, 1}, {28, 1}, {24, 0}, {41, 0} };
			int[][] frl_face_neck 		= { {40, 0}, {39, 1}, {43, 0}, {25, 1}, {28, 1}, {24, 0}, {41, 0}, {38, 0} };
			int[][] frl_face_ears 		= { {42, 0}, {43, 0}, {27, 1}, {28, 1}, {26, 0}, {41, 0} };
			int[][] frl_face_neck_ears 	= { {40, 0}, {39, 0}, {39, 1}, {43, 0}, {27, 1}, {28, 1}, {26, 0}, {41, 0}, {38, 0} };
			
			int[][] maskArray;
			
			if (mask == "frl_face_neck_ears") {
				maskArray = frl_face_neck_ears;
			} else if (mask == "frl_face_ears") {
				maskArray = frl_face_ears;
			} else if (mask == "frl_face_neck") {
				maskArray = frl_face_neck;
			} else if (mask == "frl_face") {
				maskArray = frl_face;
			} else if (mask == "pl_no_ears") {
				maskArray = pl_no_ears;
			} else if (mask == "pl_with_ears") {
				maskArray = pl_with_ears;
			} else {
				maskArray = frl_face;
			}
			
			for (int i = 0; i < maskArray.length; i++) {
				pciMask.add(maskArray[i][0], maskArray[i][1]);
			}
			fos.println("Loaded Mask " + mask + ": " + pciMask.getContours().toString());
			
			
			if (usePCI) {
				fos.println("Loading Saved PCI");
	
				String pciToLoad = mydirectory + subfolder + pcifile + ".pci";
				fos.println("--Reading PCI model: " + pciToLoad);
				f = new File(pciToLoad);
				
				if (f.exists() && !f.isDirectory()) {
					myPCI.read(pciToLoad, 100);
				} else {
					fos.println("No pci file specified");
					hasError = "true";
					errorText = "No pci file specified";
				}
			} else {
		        fos.println("Starting PCI Analysis");
		        
		        if (avgTem == null || avgImg == null) {
			        Facemorph.PCA myPCA2 = new Facemorph.PCA();
			        Facemorph.Template pcaTemplate2 = new Facemorph.Template();
			        pcaTemplate2 = myPCA2.build(temlist, ASM.RIGID_BODY_NORMALISATION, null);
			        
			        // create average image
					Image preAvgImg2 = null;
					Facemorph.Template preAvgTem2 = new Facemorph.Template();
					
					if (texture) {
		            	preAvgImg2 = Transformer.averageImagesTexture(imlist, tmlist, preAvgTem2, width, height, faceFilter, 1, null, false, ASM.RIGID_BODY_NORMALISATION);
		            } else {
		            	preAvgImg2 = Transformer.averageImages(imlist, tmlist, preAvgTem2, width, height, null, false, ASM.RIGID_BODY_NORMALISATION);
		            }
		            
		            // warp the average into the exact PCA template
		            avgImg = Transformer.testTransform(Warp.MULTISCALE, preAvgTem2, preAvgTem2, pcaTemplate2, avgTem, 
													preAvgImg2, preAvgImg2, preAvgImg2, 
													1.0, 0.0, 0.0, 
													faceFilter, 1, null, true, true,
													false, ASM.NO_NORMALISATION, null);
	
		            //write out average image
		            File uploadDir = new File(mydirectory + subfolder + "/.tmp/");
		            File a = File.createTempFile("avg", ".jpg", uploadDir);
		            BufferedImage bi = Transformer.ImageToBufferedImage(avgImg, null); 
					ImageIO.write(bi, "jpg", a);
		
					// write out average .tem file
					imgfilepath = a.getName();
					temfilepath = imgfilepath.replace(".jpg", ".tem");
					writeTem(avgTem, mydirectory + subfolder + "/.tmp/" + temfilepath);
					
					fos.println("--Created: " + imgfilepath);
					fos.println("--Created: " + temfilepath);
		        }
			
				myPCI.build(avgTem, avgImg, imlist, tmlist, pciMask, 100.0);
				
				// create directory for PCI files if needed
				String pcidirpath = mydirectory + subfolder + pcifile + ".pci";
				File pcidir = new File(pcidirpath);
				if (!pcidir.isDirectory()) { pcidir.mkdir(); }
				
				// write out pci files
				String pcifilepath = pcidirpath + "/_info.pci";
				p = Paths.get(pcifilepath);
				if (Files.deleteIfExists(p)) { fos.println("Deleted " + pcifilepath); }
				myPCI.write(pcifilepath);
				
				// write out variance table
				int ci = myPCI.getCount();
				double ctotalVars = myPCI.getTotalVariance();
				String cmyVars = "PC,variance,percent,cumulative\n";
				double ccumVars = 0.0;
				for (int i=0; i<ci; i++) {
					double v = myPCI.getVariance(i);
					double pVar = (myPCI.getVariance(i) / ctotalVars);
					ccumVars = ccumVars + pVar;
					
					cmyVars = cmyVars + "PC" + i + "," + ((double)Math.round(v * 10) / 10) +"," 
							+ ((double)Math.round(pVar * 100 * 1000) / 1000) + "," 
							+ ((double)Math.round(ccumVars * 100 * 1000) / 1000) + "\n";
				}
	
				String cvarfilepath = mydirectory + subfolder + pcifile + ".pci.vars.csv";
				p = Paths.get(cvarfilepath);
				if (Files.deleteIfExists(p)) { fos.println("--Deleted " + cvarfilepath); }
				writeTxt(cmyVars, cvarfilepath);
				fos.println("--Created: " + cvarfilepath);
				//newFiles.add(cvarfilepath.replace(mydirectory + subfolder));
			}
			
			// analyse all images if analyse variable is true
			if (analysePCI) {
				String pciAnalysis = "Image";
				
				// make header
		    	int pcn = myPCI.getCount();
		    	for (int i=0; i<pcn; i++) {
		    		pciAnalysis = pciAnalysis + ",PC" + i;
		    	}
		    	pciAnalysis = pciAnalysis + "\n";
	
				FloatImage floatMask = new FloatImage();
				//floatMask = pciMask.getAsFloatImage(avgTem, width, height, 0.0f, 1.0f);
		    	
			    for (int i=0; i<imlist.length; i++) {
				    // convert img to floatimage
				    FloatImage[] fimg = new FloatImage[3];
				    fimg[0] = new FloatImage();
				    fimg[1] = new FloatImage();
				    fimg[2] = new FloatImage();
				    
				    FloatImage.convertImageRGB(imlist[i], fimg[0], fimg[1], fimg[2], null); //RGB
				    //FloatImage.convertImageLAB(imlist[i], fimg[0], fimg[1], fimg[2], null); // LAB
	
				    float[] weights = myPCI.analyse(fimg, floatMask);
				    
				    pciAnalysis = pciAnalysis + imgNames[i] + ",";
				    for (int j=0; j<weights.length-1; j++) {
				    	pciAnalysis = pciAnalysis + weights[j] + ",";
				    }
				    pciAnalysis = pciAnalysis + weights[weights.length-1] + "\n";
				}
				
				String pcianalysisfilepath = mydirectory + subfolder + canalysisfile + ".color.csv";
				writeTxt(pciAnalysis, pcianalysisfilepath);
				fos.println("--Created: " + pcianalysisfilepath);
				//newFiles.add(pcianalysisfilepath.replace(mydirectory + subfolder));
				
			}
		}
		
		/*****************************************
		 Return error data
		 *****************************************/		
		
		String newFilesString = "";
		//for (String s : newFiles) {
		//    newFilesString += s + "\",\"";
		//}
		
		// write output	
		String jsonOutput = String.format(	
			"{ " + "\n" +
				"\"error\": %s, " + "\n" +
				"\"errorText\": \"%s\"," +  "\n" +
				"\"savefolder\": \"%s\"," + "\n" +
				"\"img\": \"%s\", " + "\n" +
				"\"tem\": \"%s\"," + "\n" +
				"\"newfiles\": [\"%s\"]" + "\n" +
			" }",
			hasError, 
			errorText,
			subfolder,
			imgfilepath,
			temfilepath,
			newFilesString
		);
			
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
    
    public void writeTem(Facemorph.Template theTem, String filename) throws ServletException, IOException {
		// write out a .tem file to a filename
		Path p = Paths.get(filename);
		Files.deleteIfExists(p);
		File t = new File(filename);
		t.createNewFile();
		FileOutputStream temout = new FileOutputStream(t);
		PrintStream tem_ps = new PrintStream(new BufferedOutputStream(temout), true);
		theTem.write(tem_ps);
		tem_ps.flush();
		tem_ps.close();
	}
	
	public void writeTxt(String theContents, String filename) throws ServletException, IOException {
		Path p = Paths.get(filename);
		Files.deleteIfExists(p);
		File f = new File(filename);			
		Writer awriter = null;
		
		try {
		    awriter = new BufferedWriter(
		    			new OutputStreamWriter(
							new FileOutputStream(filename), "utf-8"));
		    awriter.write(theContents);
		} catch (IOException ex) {
			// report
		} finally {
		   try {awriter.close();} catch (Exception ex) {}
		}
	}
}