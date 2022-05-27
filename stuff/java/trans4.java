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

public class trans4 extends HttpServlet {
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
            PrintStream fos = new PrintStream(debugFolder + "trans4_init.txt");
            fos.println("Initialising transform servlet v.4");
            mydirectory = this.getInitParameter("mydirectory");
            fos.println("    mydirectory = " + mydirectory);
            fos.println("Completed initialising transform servlet v.4");
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
        long loadImageStartTime = System.currentTimeMillis();
        Date date = new Date();
        
        // create error data for returning error info via json
        String hasError = "false";
        String errorText = "";
        
        PrintStream fos = new PrintStream(debugFolder + "trans4_debug.txt");
        fos.println("Starting transform (" + date.toString() + ")");

        // Use a single parameter called images that holds a list of images
        int count = Integer.parseInt(req.getParameter("count"));
        
        Image[] outImg = new Image[count];
        String[] transList = new String[count];
        
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
            String[] names = new String[3];
            names[0] = req.getParameter("transimage" + i);
            names[1] = req.getParameter("fromimage" + i);
            names[2] = req.getParameter("toimage" + i);
            
            Image[] imlist = new Image[names.length];
            Facemorph.Template[] tmlist = new Facemorph.Template[names.length];
            
            int loadedImages = 0;
            
            for (int j=0; j<names.length; j++) {
                // make sure there is no extension on the name
                String cleanName = names[j].replace(".jpg", "").replace(".png", "").replace(".gif", "").replace(".tem", "");
            
                // load img and tem
                fos.println("Loading NEW " + cleanName);
                
                String imgName = mydirectory + subfolder + names[j];
                String temName = mydirectory + subfolder + cleanName + ".tem";

                fos.println("    Reading template from " + temName);
                
                Facemorph.Template tem = new Facemorph.Template();
                
                File t = new File(temName);
                if(!t.exists()) {
                    fos.println("  " + temName + " does not exist: " + mydirectory + "notem.tem");
                    //hasError = "true";
                    //errorText = temName + " does not exist";
                    //break;
                    
                    // adds a 1-point template 
                    //temName = mydirectory + "notem.tem";
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
                        imagefound = true;
                        names[j] = names[j] + ext[k];
                    }
                    k++;
                } while (k<ext.length && !imagefound);
                
                if(!imagefound) {
                    fos.println("  " + imgName + " does not exist");
                    hasError = "true";
                    errorText = imgName + " does not exist";
                    break;
                }
            }
            
            if (loadedImages != 3) { 
                break; 
            }
            
            
            double shapePcnt = 0;
            if (req.getParameter("shape" + i) != null) {
                shapePcnt = Double.parseDouble(req.getParameter("shape" + i));
            }
            double colorPcnt = 0;
            if (req.getParameter("color" + i) != null) {
                colorPcnt = Double.parseDouble(req.getParameter("color" + i));
            }
            double texturePcnt = 0;
            if (req.getParameter("texture" + i) != null) {
                texturePcnt = Double.parseDouble(req.getParameter("texture" + i));
            }
            boolean sampleContours = true;
            if (req.getParameter("sampleContours" + i) != null) {
                sampleContours = Boolean.parseBoolean(req.getParameter("sampleContours" + i));
            }
            
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
                } else if (norms.equals("threepoint")) {
                    normalisation = ASM.THREE_POINT_NORMALISATION;
                    normPoints = new int[3];
                    normPoints[0] = 0;
                    normPoints[1] = 1;
                    normPoints[2] = 96;
                    
                    normPointString = normPoints[0] + "," + normPoints[1] + "," + normPoints[2];
                } else if (norms.equals("none")) {
                    normalisation = ASM.NO_NORMALISATION;
                }
            }
            
            int warpType = Warp.MULTISCALE;
            if (req.getParameter("warp" + i) != null) {
                String warp = req.getParameter("warp" + i);
                if (warp.equals("multiscale")) {
                    warpType = Warp.MULTISCALE;
                } else if (warp.equals("linear")) {
                    warpType = Warp.LINEAR;
                } else if (warp.equals("tps")) {
                    warpType = Warp.TPS;
                } else if (warp.equals("multiscalerb")) {
                    warpType = Warp.MULTISCALERB;
                }
            }
            
            fos.println("    shapePcnt[" + i + "] = " + shapePcnt);
            fos.println("    colorPcnt[" + i + "] = " + colorPcnt);
            fos.println("    texturePcnt[" + i + "] = " + texturePcnt);
            fos.println("    sampleContours[" + i + "] = " + sampleContours);
            fos.println("    norm[" + i + "] = " + normalisation);
            fos.println("    warp[" + i + "] = " + warpType);
            
            long loadImageStopTime = System.currentTimeMillis();
            fos.println("Load Images Time: " + (loadImageStopTime-loadImageStartTime));

            fos.println("Making Transform...");
            long transStartTime = System.currentTimeMillis();
            Facemorph.Template avTem = new Facemorph.Template();
            Image average = null;
            
             /*                     
            testTransform(  int warpType, Template subTem, Template srcTem, Template dstTem, Template outTem, 
                            java.awt.Image subImg, java.awt.Image srcImg, java.awt.Image dstImg, 
                            double shapeAmount, double colourAmount, double textureAmount, 
                            Filter[] filters, int bm, Mask mask, boolean samples, boolean borders, 
                            boolean matchCols, int normalisation, int[] normPoints) 
            Parameters:
                warpType - the type of warp to use, one of Warp.MULTISCALE, Warp.LINEAR, Warp.TPS or Warp.MULTISCALERB
                subTem - the subject's Template
                srcTem - the source group Template
                dstTem - the target group Template
                outTem - the output Template
                subImg - the subject image
                srcImg - the source group image
                dstImg - the target group image
                shapeAmount - the amount to transform the shape by
                colourAmount - the amount to transform the colour by
                textureAmount - the amount to transform the texture by
                filters - the filters to use for texture transformation
                bm - the border model (sym=1, anti-sym=-1) to use for the filters
                mask - the mask to use (if any, or null)
                samples - sample along template contours
                borders - pin down the borders
                matchCols - match the colours on the masked area
                normalisation - indicates the kind of normalisation to use, one of  
                    ASM.RIGID_BODY_NORMALISATION, 
                    ASM.TWO_POINT_NORMALISATION, 
                    ASM.THREE_POINT_NORMALISATION
                    ASM.NO_NORMALISATION
                normPoints - the normalisation points to use for ASM.TWO_POINT_NORMALISATION or ASM.THREE_POINT_NORMALISATION       
            */
            
            
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
            
            average = Transformer.testTransform(warpType, tmlist[0], tmlist[1], tmlist[2], avTem, 
                                                imlist[0], imlist[1], imlist[2], 
                                                shapePcnt, colorPcnt, texturePcnt, 
                                                faceFilter, 1, null, sampleContours, true,
                                                false, normalisation, normPoints);
                                                
            long transStopTime = System.currentTimeMillis();
            fos.println("Transform Time: " + (transStopTime-transStartTime));
            
            // Get the Java runtime
            Runtime runtime = Runtime.getRuntime();
            // Run the garbage collector
            runtime.gc();
            // Calculate the used memory
            long memory = runtime.totalMemory() - runtime.freeMemory();
            fos.println("Memory used: " + (memory / (1024L * 1024L)) + "MB of " 
                        + (runtime.maxMemory() / (1024L * 1024L)) + "MB");
             
            if (average==null)  {
                fos.println("  transform null");
                hasError = "true";
                errorText = "Transform " + i + " was not made";
                transList[i] = String.format(   
                    "{ " + "\n" +
                        "\"error\": %s, " + "\n" +
                        "\"errorText\": \"%s\", " + "\n" +
                        "\"transimg\": \"%s\"," + "\n" +
                        "\"fromimg\": \"%s\"," + "\n" +
                        "\"toimg\": \"%s\"," + "\n" +
                        "\"shape\": \"%s%%\"," + "\n" +
                        "\"color\": \"%s%%\"," + "\n" +
                        "\"texture\": \"%s%%\"," + "\n" +
                        "\"norm\": \"%s\"," + "\n" +
                        "\"normpoints\": \"%s\"," + "\n" +
                        "\"warp\": \"%s\"," + "\n" +
                        "\"contours\": \"%s\"," + "\n" +
                        "\"savefolder\": \"%s\"," +
                        "\"img\": \"%s\"," + "\n" +
                        "\"tem\": \"%s\"," + "\n" +
                        "\"memory\": %s," + "\n" +
                        "\"load-image-time\": %s," + "\n" +
                        "\"transform-time\": %s" + "\n" +
                    " }",
                    hasError, errorText, names[0], names[1], names[2], 
                    shapePcnt*100, colorPcnt*100, texturePcnt*100, 
                    (ASM.NO_NORMALISATION == normalisation ? "none" : 
                        (ASM.TWO_POINT_NORMALISATION == normalisation ? "2point" : 
                            (ASM.RIGID_BODY_NORMALISATION == normalisation ? "rigid" : "unknown")
                        )
                    ), normPointString,
                    (Warp.MULTISCALE == warpType ? "multiscale" : 
                        (Warp.LINEAR == warpType ? "linear" : 
                            (Warp.TPS == warpType ? "TPS" : 
                                (Warp.MULTISCALERB == warpType ? "multiscaleRB" : "unknown")
                            )
                        )
                    ),
                    sampleContours, savefolder, "null", "null",
                    (memory / (1024L * 1024L)),
                    (loadImageStopTime-loadImageStartTime),
                    (transStopTime-transStartTime)
                );
            } else {
                String myFormat = req.getParameter("format" + i);
                if (myFormat != "jpg" && myFormat != "png" && myFormat != "gif") {
                    myFormat = "jpg";
                }
                
                // write out image file
                File f = File.createTempFile("tran", "." + myFormat, uploadDir);
                BufferedImage bi = Transformer.ImageToBufferedImage(average, null); 
                ImageIO.write(bi, myFormat, f);
                int width              = bi.getWidth();
                int height             = bi.getHeight();
            
                // write out .tem file
                String tpath = f.getName().replace("." + myFormat, ".tem");
                File t = new File(uploadDir, tpath);
                t.createNewFile();
                FileOutputStream temout = new FileOutputStream(t);
                PrintStream tem_ps = new PrintStream(new BufferedOutputStream(temout), true);
                avTem.write(tem_ps);
                tem_ps.flush();
                tem_ps.close();
                
                transList[i] = String.format(   
                    "{ " + "\n" +
                        "\"error\": %s, " + "\n" +
                        "\"errorText\": \"%s\", " + "\n" +
                        "\"transimg\": \"%s\"," + "\n" +
                        "\"fromimg\": \"%s\"," + "\n" +
                        "\"toimg\": \"%s\"," + "\n" +
                        "\"shape\": \"%s%%\"," + "\n" +
                        "\"color\": \"%s%%\"," + "\n" +
                        "\"texture\": \"%s%%\"," + "\n" +
                        "\"norm\": \"%s\"," + "\n" +
                        "\"normpoints\": \"%s\"," + "\n" +
                        "\"warp\": \"%s\"," + "\n" +
                        "\"contours\": \"%s\"," + "\n" +
                        "\"savefolder\": \"%s\"," +
                        "\"img\": \"%s\"," + "\n" +
                        "\"tem\": \"%s\"," + "\n" +
                        "\"width\": %s, " + "\n" +
                        "\"height\": %s, " + "\n" +
                        "\"memory\": %s," + "\n" +
                        "\"load-image-time\": %s," + "\n" +
                        "\"transform-time\": %s" + "\n" +
                    " }",
                    hasError, errorText, names[0], names[1], names[2], 
                    shapePcnt*100, colorPcnt*100, texturePcnt*100,  
                    (ASM.NO_NORMALISATION == normalisation ? "none" : 
                        (ASM.TWO_POINT_NORMALISATION == normalisation ? "2point" : 
                            (ASM.RIGID_BODY_NORMALISATION == normalisation ? "rigid" : "unknown")
                        )
                    ), normPointString,
                    (Warp.MULTISCALE == warpType ? "multiscale" : 
                        (Warp.LINEAR == warpType ? "linear" : 
                            (Warp.TPS == warpType ? "TPS" : 
                                (Warp.MULTISCALERB == warpType ? "multiscaleRB" : "unknown")
                            )
                        )
                    ), 
                    sampleContours, savefolder, f.getName(), t.getName(),
                    width, height,
                    (memory / (1024L * 1024L)),
                    (loadImageStopTime-loadImageStartTime),
                    (transStopTime-transStartTime)
                );
            }
        }
        
        // return image and template names
        String jsonResponse = "[";
        for (int j=0; j<transList.length-1; j++) {
            if (transList[j] != null) {
                jsonResponse = jsonResponse + transList[j] + ", ";
            }
        }
        if (transList[transList.length-1] != null) {
            jsonResponse = jsonResponse + transList[transList.length-1] + "]";
        }
        
        fos.println("JSON Response: \n" + jsonResponse);

        //New print writer to write output to       
        res.setContentType("application/json");
        PrintWriter out = res.getWriter();
        out.println(jsonResponse);
        
        fos.flush();
        fos.close();
   
        out.flush();
        out.close();
    }

}
