/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.Template;
import Facemorph.Warp;
import Facemorph.psychomorph.Batchable;
import Facemorph.psychomorph.DelineatorForm;
import Facemorph.psychomorph.ImageZoomPanel;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * Batch symmetriser
 * @author bpt
 */
public class BatchSymmetrise implements Batchable {
    int[] plist;
    PsychoMorphForm psychomorph;


    @Override
    public boolean process(ImageZoomPanel izp, boolean single) {
        	Template symtemp = new Template();
		Template template = izp.getTemplate();
                BufferedImage img = DelineatorForm.checkBufferedImage(izp.getImage());

                 if (single) {
                    psychomorph.getDelineator().getImageUndoStack().push(img);
                    psychomorph.getDelineator().getTemplateUndoStack().push(template);
                    psychomorph.getDelineator().getTransformUndoMenuItem().setEnabled(true);
                }
                symtemp.symmetrise(template, plist, img.getWidth());
		symtemp.copySamples(template);
               // MultiscaleWarp warp = new MultiscaleWarp(img.getWidth(), img.getHeight());
                Warp warp = Warp.createWarp(psychomorph.getWarpType(), img.getWidth(), img.getHeight(), img.getWidth(), img.getHeight(), false);
                warp.interpolate(template, symtemp, true,  true, psychomorph.getOverlap());
		img = warp.warpImage(img);
		if (psychomorph.getDelineator().getColourCheckBoxMenuItem().getState()) {
		    symmetrise(img);
		}
		if (psychomorph.getDelineator().getShapeCheckBoxMenuItem().getState()) {
		    izp.setTemplate(symtemp);
		} else {
                     //warp = new MultiscaleWarp(img.getWidth(), img.getHeight());
                    warp = Warp.createWarp(psychomorph.getWarpType(), img.getWidth(), img.getHeight(), img.getWidth(), img.getHeight(), false);
                    warp.interpolate(symtemp, template, true, true, psychomorph.getOverlap());
                    img = warp.warpImage(img);

		}
                izp.setImage(img);
                return true;
    }

    @Override
    public boolean initialise(PsychoMorphForm psychomorph) {
        this.psychomorph = psychomorph;
        JFileChooser chooser = PsychoMorphForm.setUpFileDialog(psychomorph.getFileChooser(), "Symmetry File", "sym");
        psychomorph.setFileChooser(chooser);
        // morphApp.getFileChooser().setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    int ok = psychomorph.getFileChooser().showOpenDialog(psychomorph.getDelineator());
    File f2 = psychomorph.getFileChooser().getSelectedFile();
    plist = null;
    if (f2 == null || ok != JFileChooser.APPROVE_OPTION) return false;

    try {
        plist = Template.readSymFile(f2.getPath());
    } catch (FileNotFoundException ex) {
        // Logger.getLogger(DelineatorForm.class.getName()).log(Level.SEVERE, null, ex);
        JOptionPane.showMessageDialog(psychomorph.getDelineator(), "Error reading sym file " + f2 + ", error " + ex, "Symmetry file read error", JOptionPane.ERROR_MESSAGE);
        ex.printStackTrace();
        return false;
    } catch (IOException ex) {
        //  Logger.getLogger(DelineatorForm.class.getName()).log(Level.SEVERE, null, ex);
        JOptionPane.showMessageDialog(psychomorph.getDelineator(), "Error reading sym file " + f2 + ", error " + ex, "Symmetry file read error", JOptionPane.ERROR_MESSAGE);
        ex.printStackTrace();
        return false;
    }
    return true;
    }

    @Override
    public void finish() {
       // throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String getName() {
        return "Symmetrise";
    }

    void symmetrise(BufferedImage bimg)
{
    int x, y, r, g, b;
    int r1, g1, b1, r2, g2, b2;

    for (x=0; x<bimg.getWidth()/2; x++)
		for (y=0; y<bimg.getHeight(); y++)
		{
			int rgb1 = bimg.getRGB(x, y);//, r1, g1, b1);
                        Color c1 = new Color(rgb1);
                        r1 = c1.getRed();
                        g1 = c1.getGreen();
                        b1 = c1.getBlue();
			int rgb2 = bimg.getRGB(bimg.getWidth()-x-1, y);//, r2, g2, b2);
                        Color c2 = new Color(rgb2);
                        r2 = c2.getRed();
                        g2 = c2.getGreen();
                        b2 = c2.getBlue();
                        r = (r1+r2)/2;
			g = (g1+g2)/2;
			b = (b1+b2)/2;
                        Color c = new Color(r, g, b);
			bimg.setRGB(x, y, c.getRGB());//(unsigned char)r, (unsigned char)g, (unsigned char)b);
			bimg.setRGB(bimg.getWidth()-x-1, y, c.getRGB());//, (unsigned char)r, (unsigned char)g, (unsigned char)b);
	    }

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
