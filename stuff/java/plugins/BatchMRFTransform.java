/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.Filter;
import Facemorph.FloatImage;
import Facemorph.Pyramid;
import Facemorph.Template;
import Facemorph.Transformer;
import Facemorph.psychomorph.Average;
import Facemorph.psychomorph.Batchable;
import Facemorph.psychomorph.DelineatorForm;
import Facemorph.psychomorph.ImageZoomPanel;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.Scanner;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 *
 * @author bpt
 */
public class BatchMRFTransform implements Batchable {
    PsychoMorphForm morphApp;
    File sf, tf;
    DelineatorForm delineator;
    double[] coefs = {1, 0.5, 0};
    boolean weighted = true;

    @Override
    public boolean process(ImageZoomPanel izp, boolean single) {


        Image img = izp.getImage();
        delineator.getImageUndoStack().push(img);
        Template tem = izp.getTemplate();
        delineator.getTemplateUndoStack().push(tem);
        delineator.getTransformUndoMenuItem().setEnabled(true);

        BufferedImage bimg = Transformer.ImageToBufferedImage(img, izp);

        Average sav = new Average(morphApp, sf, true, null, morphApp.getOverlap());
        Average tav = new Average(morphApp, tf, true, null, morphApp.getOverlap());
        Template sTem = sav.getAverageTemplate();
        Template tTem = tav.getAverageTemplate();
        sav.setOutputSize(bimg.getWidth(), bimg.getHeight());
        tav.setOutputSize(bimg.getWidth(), bimg.getHeight());

 //       BufferedImage warpedTransformer.warp(null, tTem, tTem, ok, ok, rootPaneCheckingEnabled, rootPaneCheckingEnabled)
        Template subject = izp.getTemplate();
        Template warped = subject.transform(sTem, tTem, morphApp.getDelineator().getShapeSlider().getValue()/100.0, 1);
        sav.setAverageTemplate(warped);
        tav.setAverageTemplate(warped);
        /*Thread t = new Thread(sav);
        t.start();
        Thread s = new Thread(tav);
        s.start();
         */
       DelineatorForm.batchProcess(sav, "_aligned", delineator, morphApp.getFileChooser(), sf, morphApp.getLeftImagePanel());//, false, false, true);
       BufferedImage simg = Transformer.ImageToBufferedImage(morphApp.getRightImage(), izp);

       DelineatorForm.batchProcess(tav, "_aligned", delineator, morphApp.getFileChooser(), tf, morphApp.getLeftImagePanel());//, false, false, true);
       BufferedImage timg = Transformer.ImageToBufferedImage(morphApp.getRightImage(), izp);

       ArrayList<FloatImage> source = sav.getFloatImages();
       ArrayList<FloatImage> target = tav.getFloatImages();

       FloatImage Y = new FloatImage(), U = new FloatImage(), V = new FloatImage();
       Template temp = new Template();
       BufferedImage warpImg = Transformer.warp(morphApp.getWarpType(), bimg, subject, warped, bimg.getWidth(), bimg.getHeight(), true, false);
       //BufferedImage outImg = Transformer.ImageToBufferedImage(Transformer.transform(getWarpType(), warped, warped, warped, temp, warpImg, simg, timg, this.colourSlider.getValue()/100.0, izp, false), izp);
       BufferedImage outImg = Transformer.ImageToBufferedImage(
               Transformer.testTransform(morphApp.getWarpType(), warped, warped, warped, temp, warpImg,
               simg, timg, delineator.getShapeSlider().getValue()/100.0, delineator.getColourSlider().getValue()/100.0,
               0.0, morphApp.getFilters(), morphApp.getBorderModel(), izp.getMask(), true, true,
               false, morphApp.getNormalisation(),izp.getNormPoints()), izp);
       //warp(bimg, subject, warped, bimg.getWidth(), bimg.getHeight(), true, false);

       FloatImage.convertImageYUV(outImg, Y, U, V, null);
       Filter[] filters = morphApp.getFilters();//{H1, G, H2, L1, K1, L2, K2}
       int levs = (int)(Math.log((double)bimg.getWidth())/Math.log(2.0));
       Y.write("Y.fimg");
    //   Y.MultiscaleBlendWaveletMRF_fast2(source, count, levs, filters[0], filters[1], filters[2], filters[4], filters[6], filters[3], filters[5]);//H1, G, H2, K1, K2, L1, L2);
       boolean pyrmrf=true;
        if (pyrmrf) {
            ArrayList<Pyramid> srcPyr = new ArrayList<Pyramid>();
            ArrayList<Pyramid> trgPyr = new ArrayList<Pyramid>();
            levs = (int) (Math.log((double) Y.getWidth()) / Math.log(2.0));
            for (FloatImage fimg : source) {
                Pyramid pyr = new Pyramid();
                pyr.setImage(fimg);
                pyr.build_pyramid(filters[0], filters[1], levs - 1);
                srcPyr.add(pyr);
            }

            for (FloatImage fimg : target) {
                Pyramid pyr = new Pyramid();
                pyr.setImage(fimg);
                pyr.build_pyramid(filters[0], filters[1], levs - 1);
                trgPyr.add(pyr);
            }
            Pyramid pyr = new Pyramid();
            Pyramid origPyr = new Pyramid();
            origPyr.setImage(Y);
            origPyr.build_pyramid(filters[0], filters[1], levs - 1);
            pyr.MRFTransform(origPyr, srcPyr, trgPyr, -256.0f, 256.0f, 512, coefs, weighted);
            pyr.collapse_pyramid(filters[2], filters[3], filters[4], filters[5], filters[6], morphApp.getBorderModel());
           // pyr.smooth.write("tran.fimg");

            outImg = FloatImage.reconvertImageYUV(pyr.smooth, U, V);
        } else {
            FloatImage tran = new FloatImage(Y.getWidth(), Y.getHeight());
            tran.MultiscaleTransformWaveletMRF_fast2(Y, source, target, levs, filters[0], filters[1], filters[2], filters[4], filters[6], filters[3], filters[5]);

          //  tran.write("tran.fimg");
            outImg = FloatImage.reconvertImageYUV(tran, U, V);
        }
       izp.setImage(outImg);
       izp.setTemplate(warped);
       izp.paintImmediately(0, 0, izp.getWidth(), izp.getHeight());
       return true;
    }

    @Override
    public boolean initialise(PsychoMorphForm psychomorph) {
        morphApp = psychomorph;
        delineator = morphApp.getDelineator();
        JFileChooser chooser =  PsychoMorphForm.setUpFileDialog(psychomorph.getFileChooser(), "Source group list file", "txt");
        morphApp.setFileChooser(chooser);

        int ok = morphApp.getFileChooser().showOpenDialog(psychomorph);
        sf = morphApp.getFileChooser().getSelectedFile();
        if (sf==null || ok!=JFileChooser.APPROVE_OPTION) return false;

        chooser =  PsychoMorphForm.setUpFileDialog(psychomorph.getFileChooser(), "Target group list file", "txt");
        morphApp.setFileChooser(chooser);

        ok = morphApp.getFileChooser().showOpenDialog(psychomorph);
        tf = morphApp.getFileChooser().getSelectedFile();
        if (tf==null || ok!=JFileChooser.APPROVE_OPTION) return false;
        String coefStr = JOptionPane.showInputDialog(delineator, "Please specify the smoothing equation coefficients (constant linear quadratic)",  coefs[0] + " " + coefs[1] + " " + coefs[2]);
        Scanner sc = new Scanner(coefStr);
        for (int i=0; i<3; i++) {
            coefs[i] = sc.nextDouble();
        }
        int op = JOptionPane.showConfirmDialog(delineator, "Use Gaussian approximation?", "Gaussian approximation", JOptionPane.YES_NO_CANCEL_OPTION);//, JOptionPane.QUESTION_MESSAGE, null, null, null);
        switch (op) {
            case JOptionPane.CANCEL_OPTION: return false;
            case JOptionPane.YES_OPTION: weighted = true; break;
            case JOptionPane.NO_OPTION: weighted = false; break;
            default: break;
        }
        return true;
    }

    @Override
    public void finish() {
     //   throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String getName() {
        return "Transform MRF";
    }

    public boolean getReadTemplate() {
        return true;
    }

    public boolean getWriteTemplate() {
        return true;
    }

    public boolean getWriteImage() {
        return true;
    }

    
}
