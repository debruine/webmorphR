/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.Template;
import Facemorph.Transformer;
import Facemorph.psychomorph.Batchable;
import Facemorph.psychomorph.DelineatorForm;
import Facemorph.psychomorph.ImageZoomPanel;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.Image;
import java.awt.image.BufferedImage;
import javax.swing.JOptionPane;

/**
 * Chimeric transform implementation
 * @author bpt
 */
public class BatchChimericTransform implements Batchable {
    PsychoMorphForm morphApp;
    float l1, l2, wid;

    @Override
    public boolean process(ImageZoomPanel izp, boolean single) {
        BufferedImage leftImage = DelineatorForm.checkBufferedImage(morphApp.getLeftImage());
    Template leftTemplate = morphApp.getLeftTemplate();
    BufferedImage rightImage = DelineatorForm.checkBufferedImage(morphApp.getRightImage());
    Template rightTemplate = morphApp.getRightTemplate();
    BufferedImage img = DelineatorForm.checkBufferedImage(izp.getImage());

    Template tem = izp.getTemplate();
    Template outTem = new Template();

    if (single) {
        morphApp.getDelineator().getImageUndoStack().push(img);
        morphApp.getDelineator().getTemplateUndoStack().push(tem);
        morphApp.getDelineator().getTransformUndoMenuItem().setEnabled(true);
    }
   
    // transformChimeric(Template subTem, Template srcTem, Template dstTem, Template outTem,
    //BufferedImage subImg, BufferedImage srcImg, BufferedImage dstImg, float leftAmount, float rightAmount, float width, boolean samples, boolean borders) {

    Image outImg = Transformer.transformChimeric(morphApp.getWarpType(), tem, leftTemplate, rightTemplate, outTem,
            img, leftImage, rightImage, l1, l2, wid, true, false);
    izp.setImage(outImg);
    izp.setTemplate(outTem);
    return true;
    }

    @Override
    public void finish() {
        //throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String getName() {
        return "Transform Chimeric";
    }

    @Override
    public boolean initialise(PsychoMorphForm psychomorph) {
        morphApp = psychomorph;

        String leftStr = JOptionPane.showInputDialog(morphApp.getDelineator(), "Transform level for left side", "1");
        String rightStr = JOptionPane.showInputDialog(morphApp.getDelineator(), "Transform level for right side", "0");
        int w = 50;//(int)(tem.getPoint(0).distance(tem.getPoint(1))/10.0);
        String widthStr = JOptionPane.showInputDialog(morphApp.getDelineator(), "Width of smoothing band", "" + w);
        l1 = Float.parseFloat(leftStr);
        l2 = Float.parseFloat(rightStr);
        wid = Float.parseFloat(widthStr);
        return true;
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
