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
import Facemorph.*;

public class avg4 extends HttpServlet {
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
            PrintStream fos = new PrintStream(debugFolder + "avg4_init.txt");
            fos.println("Initialising average servlet v.4");
            mydirectory = this.getInitParameter("mydirectory");
            fos.println("    mydirectory = " + mydirectory);
            fos.println("Completed initialising average servlet v.4");
            fos.flush();
            fos.close();
        } catch (IOException e) {
            System.out.println(e);
            throw new ServletException(e);
        }
    }
 
       
    public void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
        doGet(req,res);
    }
    
    
    public void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
        Date date = new Date();
        long loadImageStartTime = System.currentTimeMillis();
        
        // create error data for returning error info via json
        String hasError = "false";
        String errorText = "";
    
        PrintStream fos = new PrintStream(debugFolder + "avg4_debug.txt");
        fos.println("Starting average (" + date.toString() + ")");

        // how many averages to make, default to 1
        int count = 1;      
        if (req.getParameter("count") != null) {
            count = Integer.parseInt(req.getParameter("count"));
        }
        
        Image[] outImg = new Image[count];
        String[] avgList = new String[count];
        
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
        String[] ext = {"", ".jpg", ".png", ".gif"};
        
        for(int i=0; i<count; i++) {
            if (req.getParameterValues("images" + i) == null) {
                hasError = "true";
                errorText = "No images listed";
                break;
            }
            
            String[] names = req.getParameterValues("images" + i);
            
            int width = 0;
            int height = 0;
            if (req.getParameter("width" + i) != null && req.getParameter("height" + i) != null) {
                width = Integer.parseInt(req.getParameter("width" + i));
                height = Integer.parseInt(req.getParameter("height" + i));  
            } else {
                String firstImageName = mydirectory + subfolder + names[0];
                fos.println("Loading first image: " + firstImageName);
                BufferedImage img1 = ImageIO.read(new File(firstImageName));
                width              = img1.getWidth();
                height             = img1.getHeight();
            }
            
            boolean texture = false;
            if (req.getParameter("texture" + i) != null) {     
                texture = Boolean.parseBoolean(req.getParameter("texture" + i));
            }
            boolean autosize = false; // may add as an option in the future
            
            // normalisation, defaults to none
            int normalisation = ASM.NO_NORMALISATION;
            int[] normPoints = null;
            String normPointString = "";
            if (req.getParameter("norm" + i) != null) {
                String norms = req.getParameter("norm" + i);
                
                if (norms.equals("rigid")) {
                    normalisation = ASM.RIGID_BODY_NORMALISATION;
                } else if (norms.equals("twopoint")) {
                    normalisation = ASM.TWO_POINT_NORMALISATION;
                    normPoints = new int[2];
                    if (req.getParameter("normPoint0_" + i) != null && req.getParameter("normPoint1_" + i) != null) {
                        normPoints[0] = Integer.parseInt(req.getParameter("normPoint0_" + i));
                        normPoints[1] = Integer.parseInt(req.getParameter("normPoint1_" + i));
                    } else {
                        normPoints[0] = 0;
                        normPoints[1] = 1;
                    }
                    normPointString = normPoints[0] + "," + normPoints[1];
                } else if (norms.equals("none")) {
                    normalisation = ASM.NO_NORMALISATION;
                }
            }
            
            Image[] imlist = new Image[names.length];
            Facemorph.Template[] tmlist = new Facemorph.Template[names.length];
            
            fos.println("width[" + i + "] = " + width);
            fos.println("height[" + i + "] = " + height);
            fos.println("texture[" + i + "] = " + texture);
            fos.println("norm[" + i + "] = " + normalisation);
            fos.println("names[" + i + "].length = " + names.length);
            
            if (names.length == 0) {
                fos.println("Error: no images loaded");
                hasError = "true";
                errorText = "Average " + i + " was not made because no images were loaded";
                break;
            }
            
            String imageNameList = "";
            int loadedImages = 0;
            
            for (int j=0; j<names.length; j++) {
                // make sure there is no extension on the name
                String cleanName = names[j].replace(".jpg", "")
                                           .replace(".png", "")
                                           .replace(".gif", "")
                                           .replace(".tem", "");
            
                // load img and tem
                fos.println("Loading " + names[j]);
                
                String imgName = mydirectory + subfolder + names[j];
                String temName = mydirectory + subfolder + cleanName + ".tem";

                fos.println("    Reading template from " + temName);
                
                Facemorph.Template tem = new Facemorph.Template();
                
                File t = new File(temName);
                if(!t.exists()) {
                    fos.println("  " + temName + " does not exist");
                    //hasError = "true";
                    //errorText = temName + " does not exist";
                    //break;
                    
                    tem.addPoint(0, 0);
                } else {
                    tem.read(temName);
                }

                fos.println("    Reading image from " + imgName);
                int k=0;
                boolean imagefound = false;
                
                do {
                    File p = new File(imgName + ext[k]);
                    if(p.exists()) {
                        ImageIcon ii = new ImageIcon(imgName + ext[k]);
                        Image img = ii.getImage();
                        imlist[j] = img;
                        tmlist[j] = tem;
                        
                        loadedImages++;
                        imageNameList = imageNameList + "\"" + names[j] + ext[k] + "\",";
                        imagefound = true;
                    }
                    k++;
                } while (k<ext.length && !imagefound); // checks through possible extensions until a file is found
                
                if(!imagefound) {
                    fos.println("  " + imgName + " does not exist");
                    hasError = "true";
                    errorText = imgName + " does not exist";
                    break;
                }

            }
            imageNameList = imageNameList.substring(0, imageNameList.length() - 1);
            
            if (loadedImages == 0) { 
                break; 
            }
            
            long loadImageStopTime = System.currentTimeMillis();
            fos.println("Load Images Time: " + (loadImageStopTime-loadImageStartTime));
            
            fos.println("Making Average " + i);
            long averageStartTime = System.currentTimeMillis();
            Facemorph.Template avTem = new Facemorph.Template();
            Image average = null;
            
           /* FUNCTION REFERENCE
           
           averageImagesTexture(  java.awt.Image[] images,
                                  Template[] templates,
                                  Template averageTemplate,
                                  int outW,
                                  int outH,
                                  Filter[] filters,
                                  int bm,
                                  java.awt.image.ImageObserver obs,
                                  boolean autoSize,
                                  int normalisation)
                                  
                images - The array of images to be averaged.
                templates - The array of templates outlining the features of the corresponding templates.
                averageTemplate - The template in which the average shape description will be placed.
                outW - The desired width of the output image.
                outH - The desired height of the output image.
                filters - the filters to use
                bm - the border model to use for the filters
                obs - An image observer used for getting the height and width of the image
                autoSize - indicates that the template should be expanded to fill the output image
                normalisation - the type of normalisation to use 
                    ASM.RIGID_BODY_NORMALISATION (rigid body + scale (procrustes) normalisation)
                    ASM.TWO_POINT_NORMALISATION
                    ASM.NO_NORMALISATION 
           
           averageImages(  java.awt.Image[] images,
                           Template[] templates,
                           Template averageTemplate,
                           int outW,
                           int outH,
                           java.awt.image.ImageObserver obs,
                           boolean autoSize,
                           int normalisation)
            */
            
            if (texture) {
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
                
                average = Transformer.averageImagesTexture(imlist, tmlist, avTem, width, height, faceFilter, 1, null, autosize, normalisation);
            } else {
                average = Transformer.averageImages(imlist, tmlist, avTem, width, height, null, autosize, normalisation);
            }
            
            long averageStopTime = System.currentTimeMillis();
            fos.println("Average Images Time: " + (averageStopTime-averageStartTime));
            
            // Get the Java runtime
            Runtime runtime = Runtime.getRuntime();
            // Run the garbage collector
            runtime.gc();
            // Calculate the used memory
            long memory = runtime.totalMemory() - runtime.freeMemory();
            fos.println("Memory used: " + (memory / (1024L * 1024L)) + "MB of " 
                        + (runtime.maxMemory() / (1024L * 1024L)) + "MB");
            
            if (average==null) {
                 fos.println("  average null");
                 hasError = "true";
                 errorText = "Average " + i + " was not made";
                 
                 avgList[i] = String.format(    
                    "{ " + "\n" +
                        "\"error\": %s, " + "\n" +
                        "\"errorText\": \"%s\", " + "\n" +
                        "\"texture\": \"%s\"," + "\n" +
                        "\"norm\": \"%s\"," + "\n" +
                        "\"normpoints\": \"%s\"," + "\n" +
                        "\"images\": [%s], " + "\n" +
                        "\"memory\": %s" + "\n" +
                    " }",
                    hasError, 
                    errorText, 
                    texture, 
                    (ASM.NO_NORMALISATION == normalisation ? "none" : 
                        (ASM.TWO_POINT_NORMALISATION == normalisation ? "2point" : 
                            (ASM.RIGID_BODY_NORMALISATION == normalisation ? "rigid" : "unknown")
                        )
                    ), normPointString,
                    imageNameList,
                    (memory / (1024L * 1024L))
                );
            } else {
                String myFormat = req.getParameter("format" + i);
                if (myFormat != "jpg" && myFormat != "png" && myFormat != "gif") {
                    myFormat = "jpg";
                }
                fos.println("Format = " + myFormat);
                
                File f = File.createTempFile("avg", "." + myFormat, uploadDir);
                BufferedImage bi = Transformer.ImageToBufferedImage(average, null); 
                ImageIO.write(bi, myFormat, f);
            
                // write out .tem file
                String tpath = f.getName().replace("." + myFormat, ".tem");
                File t = new File(uploadDir, tpath);
                t.createNewFile();
                FileOutputStream temout = new FileOutputStream(t);
                PrintStream tem_ps = new PrintStream(new BufferedOutputStream(temout), true);
                avTem.write(tem_ps);
                tem_ps.flush();
                tem_ps.close();
                
                avgList[i] = String.format( 
                    "{ " + "\n" +
                        "\"error\": %s, " + "\n" +
                        "\"errorText\": \"%s\", " + "\n" +
                        "\"texture\": \"%s\"," + "\n" +
                        "\"norm\": \"%s\"," + "\n" +
                        "\"normpoints\": \"%s\"," + "\n" +
                        "\"images\": [%s], " + "\n" +
                        "\"width\": %s, " + "\n" +
                        "\"height\": %s, " + "\n" +
                        "\"savefolder\": \"%s\"," +
                        "\"img\": \"%s\", " + "\n" +
                        "\"tem\": \"%s\"," + "\n" +
                        "\"memory\": %s," + "\n" +
                        "\"load-image-time\": %s," + "\n" +
                        "\"average-time\": %s" + "\n" +
                    " }",
                    hasError, 
                    errorText, 
                    texture, 
                    (ASM.NO_NORMALISATION == normalisation ? "none" : 
                        (ASM.TWO_POINT_NORMALISATION == normalisation ? "2point" : 
                            (ASM.RIGID_BODY_NORMALISATION == normalisation ? "rigid" : "unknown")
                        )
                    ), normPointString,
                    imageNameList,
                    width,
                    height,
                    savefolder,
                    f.getName(),
                    t.getName(),
                    (memory / (1024L * 1024L)),
                    (loadImageStopTime-loadImageStartTime),
                    (averageStopTime-averageStartTime)
                );
            }
        }
        
        // return image and template names
        String jsonResponse = "[";
        for (int j=0; j<avgList.length-1; j++) {
            if (avgList[j] != null) {
                jsonResponse = jsonResponse + avgList[j] + ", ";
            }
        }
        if (avgList[avgList.length-1] != null) {
            jsonResponse = jsonResponse + avgList[avgList.length-1] + "]";
        }

        fos.println("JSON Response: \n" + jsonResponse);

        //New print writer to write output to       
        res.setContentType("application/json");
        PrintWriter out = res.getWriter();
        out.println(jsonResponse);
        
        out.flush();
        out.close();
        
        fos.flush();
        fos.close();
    }
}