/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package plugins;

import Facemorph.BigMat;
import Facemorph.Gaussian;
import Facemorph.Transformer;
import Facemorph.psychomorph.Batchable;
import Facemorph.psychomorph.DelineatorForm;
import Facemorph.psychomorph.ImageZoomPanel;
import Facemorph.psychomorph.PsychoMorphForm;
import java.awt.Color;
import java.awt.Rectangle;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;
import java.util.Vector;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * Colour calibration implementation
 * Reads calibration chart data from file.
 * User selects a box around the chart, attempts to refine the alignment (rigid+scale) based on matching to the mean colours.
 * Optionally will refine the alignment to each image in a set, based on the initial box selection.
 * @author bpt
 */
public class BatchColourCalibrate implements Batchable {
    /** reference white D50 in XYZ coordinates */
    public static final double[] D50 = {96.4212, 100.0, 82.5188};
    /** reference white D55 in XYZ coordinates */
    public static final double[] D55 = {95.6797, 100.0, 92.1481};
    /** reference white D65 in XYZ coordinates */
    public static final double[] D65 = {95.0429, 100.0, 108.8900};
    /** reference white D75 in XYZ coordinates */
    public static final double[] D75 = {94.9722, 100.0, 122.6394};
    double[] whitePoint = D65;


double[][] spect={{38.25,	12.9,	13.1},
{64.48,	13.48,	17.16},
{51.41,	-3.98,	-20.47},
{43.07,	-11.07,	20.64},
{55.95,	6.89,	-23.33},
{71.26,	-29.71,	2.1},
{60.65,	33.43,	52.65},
{43.78,	7.58,	-40.38},
{49.68,	41.5,	13.58},
{31.61,	18.65,	-21.02},
{71.26,	-19.92,	56.62},
{68.85,	18.96,	62.97},
{33.12,	11.9,	-42.25},
{54.94,	-34.45,	33.18},
{40.81,	47.21,	22.18},
{78.08,	3.8,	76.12},
{51.7,	42.41,	-16.02},
{54.02,	-27.81,	-22.42},
{96.07,	-0.74,	1.73},
{80.39,	0.09,	-0.06},
{66.34,	-0.16,	-0.63},
{53.88,	-0.03,	-0.03},
{40.89,	0.06,	-0.26},
{28.99,	-0.1,	-0.87}};

double[][] rgbgrid={{255.0, 255.0, 255.0},
{0, 0, 142},
{255, 116, 21},
{94, 28, 13},
{249, 249, 249},
{64, 173, 38},
{7, 47, 122},
{241, 149, 108},
{180, 180, 180},
{203, 0, 0},
{222, 29, 42},
{97, 119, 171},
{117, 117, 117},
{255, 217, 0},
{69, 0, 68},
{90, 103, 39},
{53, 53, 53},
{207, 3, 124},
{187, 255, 19},
{164, 131, 196},
{0, 0, 0},
{0, 148, 189},
{255, 142, 19},
{140, 253, 153}};


BigMat B;
Gaussian[] colHist = new Gaussian[24];
PsychoMorphForm psychomorph;
double[] rigidParams = {1, 0, 0, 0};
 boolean drawCircles = true;
        boolean apply = true;
        boolean recalibrate = true;


    @Override
    public boolean process(ImageZoomPanel izp, boolean single) {

        Rectangle rect = izp.getRectangle();
        BufferedImage bimg = Transformer.ImageToBufferedImage(izp.getImage(), izp);//DelineatorForm.checkBufferedImage(izp.getImage());

        if (single) {
                    psychomorph.getDelineator().getImageUndoStack().push(izp.getImage());
                    psychomorph.getDelineator().getTemplateUndoStack().push(izp.getTemplate());
                    psychomorph.getDelineator().getTransformUndoMenuItem().setEnabled(true);
                }
        
        
        
       // ICC_ColorSpace colorSpace = new ICC_ColorSpace(ICC_Profile.getInstance(ICC_ColorSpace.CS_CIEXYZ));
        /*for (int i=0; i<spect.length; i++) {
            double[] xyz = spect[i];
            System.out.println("lab = " + xyz[0] + ", " + xyz[1] + ", " + xyz[2]);
            xyz = LabtoXYZ(spect[i], whitePoint);
            System.out.println("XYZ = " + xyz[0] + ", " + xyz[1] + ", " + xyz[2]);

            //float[] xyzf = {(float)xyz[0], (float)xyz[1], (float)xyz[2]};
            //float[] rgbf = colorSpace.toRGB(xyzf);
            //float[] rgbf = new float[3];
           /* double[] rgb = new double[3];
            rgb[0] = 3.240479*xyz[0] -1.537150*xyz[1] -0.498535*xyz[2];
            rgb[1] = -0.969256*xyz[0] + 1.875992*xyz[1] +  0.041556*xyz[2];
            rgb[2] = 0.055648*xyz[0] -0.204043*xyz[1] +  1.057311 *xyz[2];
            * /
            int[] rgb = XYZtoRGB(xyz);
            System.out.println("rgb = " + rgb[0] + ", " + rgb[1] + ", " + rgb[2]);
        }
*/

        if (recalibrate) {
             for (int i=0; i<5; i++) {
            updateRigidParams(bimg, rect);
            estimateGaussians(bimg, rect);
        }
        }
        if (apply) {
        for (int x=0; x<bimg.getWidth(); x++) {
            for (int y=0; y<bimg.getHeight(); y++) {
                int col = bimg.getRGB(x, y);
                Color c = new Color(col);
                double[] rgb = {(double)c.getRed(), (double)c.getGreen(), (double)c.getBlue()};
                double[] poly = rgb2rgbpoly(rgb);
                double[] xyz = B.multiplyTranspose(poly);
               /* float[] xyzf = {(float)xyz[0], (float)xyz[1], (float)xyz[2]};
                float[] rgbf = colorSpace.toRGB(xyzf);
                int r = rgbf[0]<0 ? 0 : (rgbf[0]>255 ? 255 : (int)rgbf[0]);
                int g = rgbf[1]<0 ? 0 : (rgbf[1]>255 ? 255 : (int)rgbf[1]);
                int b = rgbf[2]<0 ? 0 : (rgbf[2]>255 ? 255 : (int)rgbf[2]);
                c = new Color(rgbf[0], rgbf[1], rgbf[2]);*/
                int[] rgbi = XYZtoRGB(LabtoXYZ(xyz, whitePoint));
                c = new Color(rgbi[0], rgbi[1], rgbi[2]);
                /*int r = xyz[0]<0 ? 0 : (xyz[0]>255 ? 255 : (int)xyz[0]);
                int g = xyz[1]<0 ? 0 : (xyz[1]>255 ? 255 : (int)xyz[1]);
                int b = xyz[2]<0 ? 0 : (xyz[2]>255 ? 255 : (int)xyz[2]);
                c = new Color(r, g, b);
                */
                bimg.setRGB(x, y, c.getRGB());
            }
        }
        }

        if (drawCircles) {
        double scale = Math.sqrt(rigidParams[0]*rigidParams[0]+rigidParams[1]*rigidParams[1]);
        double theta = Math.atan2(rigidParams[1], rigidParams[0]);

        int cellwidth = rect.width/4, cellheight=rect.height/6;
        double radius = scale * cellwidth/3.0;

        int k=0;
        
            for (int i=0; i<4; i++)
                for (int j=0; j<6; j++)
             {
               // double[] rgb = colHist[k].getMean();//rgbgrid[k];//
               // Color col = new Color((int)(rgb[0]), (int)(rgb[1]), (int)(rgb[2]));//Color.red;//
                int[] rgbi = XYZtoRGB(LabtoXYZ(spect[k], whitePoint));
                Color col = new Color(rgbi[0], rgbi[1], rgbi[2]);
                int X = rect.x+cellwidth*i+cellwidth/2;
                int Y = rect.y+cellheight*j+cellheight/2;
                double l = rigidParams[0]*X-rigidParams[1]*Y+rigidParams[2];
                double m = rigidParams[1]*X+rigidParams[0]*Y+rigidParams[3];

                Ellipse2D.Double circ = new Ellipse2D.Double(l-radius, m-radius, 2*radius, 2*radius);
                for (int x=(int)(l-radius); x<(int)(l+radius+1); x++) {
                    for (int y=(int)(m-radius); y<(int)(m+radius+1); y++) {
                       if (circ.contains((double)x, (double)y)) {
                             bimg.setRGB(x, y, col.getRGB());
                       }
                    }
                }
                           /*
                for (int x=X; x<X+cellwidth/2; x++) {
                    for (int y=Y; y<Y+cellheight/2; y++) {
                       bimg.setRGB(x, y, col.getRGB());
                    }
                }*/
                k++;
            }
        }
        izp.setImage(bimg);

         return true;
    }

    @Override
    public boolean initialise(PsychoMorphForm psychomorph) {
        this.psychomorph = psychomorph;
        DelineatorForm delineator = psychomorph.getDelineator();
        JFileChooser chooser = PsychoMorphForm.setUpFileDialog(psychomorph.getFileChooser(), "Spectrum File", "txt");
        psychomorph.setFileChooser(chooser);


    int ok = psychomorph.getFileChooser().showOpenDialog(psychomorph.getDelineator());
    File f2 = psychomorph.getFileChooser().getSelectedFile();

    if (f2 == null || ok != JFileChooser.APPROVE_OPTION) return false;
    try {
        if (!readSpect(f2)) {
            JOptionPane.showMessageDialog(delineator, "Error unknown whitepoint (D number not 50, 55, 65 or 75) in " + f2);
            return false;
        }
    } catch (IOException ex) {
        JOptionPane.showMessageDialog(delineator, "Error reading: " + f2);
        ex.printStackTrace();
        return false;
    }

    int res = JOptionPane.showConfirmDialog(delineator, "Re-estimate calibration for each image?", "Re-estimate?", JOptionPane.YES_NO_CANCEL_OPTION);
        if (res==JOptionPane.CANCEL_OPTION) return false;
        if (res==JOptionPane.NO_OPTION) recalibrate=false;
        else recalibrate = true;

        res = JOptionPane.showConfirmDialog(delineator, "Display target colours on grid?", "Display target colours?", JOptionPane.YES_NO_CANCEL_OPTION);
        if (res==JOptionPane.CANCEL_OPTION) return false;
        if (res==JOptionPane.NO_OPTION) drawCircles=false;
        else drawCircles = true;
        ImageZoomPanel izp = delineator.getIZP();
        Rectangle rect = izp.getRectangle();
        BufferedImage bimg = DelineatorForm.checkBufferedImage(izp.getImage());
        estimateGaussians(bimg, rect);
        for (int i=0; i<5; i++) {
            updateRigidParams(bimg, rect);
            estimateGaussians(bimg, rect);
        }
        /*
        int cellwidth = rect.width/4, cellheight=rect.height/6;
        int k=0;
        
            for (int i=0; i<4; i++)
                for (int j=0; j<6; j++)
             {
                colHist[k] = new Gaussian(3);
                Vector<double[]> samples = new Vector<double[]>();
                int X = rect.x+cellwidth*i+cellwidth/4;
                int Y = rect.y+cellheight*j+cellheight/4;
                for (int x=X; x<X+cellwidth/2; x++) {
                    for (int y=Y; y<Y+cellheight/2; y++) {
                       int col = bimg.getRGB(x, y);
                       Color c = new Color(col);
                       double[] vec = {(double)c.getRed(), (double)c.getGreen(), (double)c.getBlue()};
                       samples.add(vec);
                    }
                }
                colHist[k].build(samples);
                k++;
            }
     */
        solveLinear();
      //  checkResult();
        return true;
    }

    boolean readSpect(File file) throws FileNotFoundException {
        Scanner sc = new Scanner(file);
      /*  while (sc.hasNextLine()) {
            String line = sc.nextLine();
            Scanner sc2 = new Scanner(line);
            while (sc2.hasNextDouble()) {
                sc2
            }
        }*/

        int dnum = sc.nextInt();
        switch(dnum) {
            case 50: whitePoint=D50; break;
            case 55: whitePoint=D55; break;
            case 65: whitePoint=D65; break;
            case 75: whitePoint=D75; break;
            default: return false;
        }
        for (int i=0; i<24; i++) {
            for (int j=0; j<3; j++) {
                spect[i][j]=sc.nextDouble();
            }
        }
        return true;
    }

    void estimateGaussians(BufferedImage bimg, Rectangle rect) {
        double scale = Math.sqrt(rigidParams[0]*rigidParams[0]+rigidParams[1]*rigidParams[1]);
     //   double theta = Math.atan2(rigidParams[1], rigidParams[0]);

        int cellwidth = rect.width/4, cellheight=rect.height/6;
        double radius = scale * cellwidth/3.0;


        int k=0;

            for (int i=0; i<4; i++)
                for (int j=0; j<6; j++)
             {
                colHist[k] = new Gaussian(3);
                Vector<double[]> samples = new Vector<double[]>();
                int X = rect.x+cellwidth*i+cellwidth/2;
                int Y = rect.y+cellheight*j+cellheight/2;
                double l = rigidParams[0]*X-rigidParams[1]*Y+rigidParams[2];
                double m = rigidParams[1]*X+rigidParams[0]*Y+rigidParams[3];

                Ellipse2D.Double circ = new Ellipse2D.Double(l-radius, m-radius, 2*radius, 2*radius);

                for (int x=(int)(l-radius); x<(int)(l+radius+1); x++) {
                    for (int y=(int)(m-radius); y<(int)(m+radius+1); y++) {
                       if (circ.contains((double)x, (double)y)) {
                            int col = bimg.getRGB(x, y);
                            Color c = new Color(col);
                            double[] vec = {(double)c.getRed(), (double)c.getGreen(), (double)c.getBlue()};
                            samples.add(vec);
                       }
                    }
                }
                colHist[k].build(samples);
                k++;
            }
    }

    void updateRigidParams(BufferedImage bimg, Rectangle rect) {
        BigMat A = new BigMat(4, 4);
        double[] rhs = new double[4];

        double scale = Math.sqrt(rigidParams[0]*rigidParams[0]+rigidParams[1]*rigidParams[1]);
        //double theta = Math.atan2(rigidParams[1], rigidParams[0]);

        int cellwidth = rect.width/4, cellheight=rect.height/6;
        double radius = scale * cellwidth/2.0;

        int k=0;

            for (int i=0; i<4; i++)
                for (int j=0; j<6; j++)
             {
                int X = rect.x+cellwidth*i+cellwidth/2;
                int Y = rect.y+cellheight*j+cellheight/2;
                double l = rigidParams[0]*X-rigidParams[1]*Y+rigidParams[2];
                double m = rigidParams[1]*X+rigidParams[0]*Y+rigidParams[3];

                Ellipse2D.Double circ = new Ellipse2D.Double(l-radius, m-radius, 2*radius, 2*radius);
                double[] p = getProb(bimg, colHist[k], circ);
                double x_i = X, y_i=Y, Cxx_i = p[2], Cyy_i = p[3], Cxy_i=p[4], mx_i=p[0], my_i=p[1];
                double a =  Cxx_i*x_i*x_i + 2*Cxy_i*x_i*y_i + Cyy_i*y_i*y_i;
                double b = -Cxx_i*x_i*y_i + Cxy_i*x_i*x_i - Cxy_i*y_i*y_i + Cyy_i*x_i*y_i;
                double c = Cxx_i*x_i + Cxy_i*y_i;
                double d = Cxy_i*x_i + Cyy_i*y_i;
                double cnst = -Cxx_i*mx_i*x_i - Cxy_i*my_i*x_i - Cxy_i*mx_i*y_i - Cyy_i*my_i*y_i;
                A.add(0, 0, a); A.add(0,1,b); A.add(0,2, c); A.add(0,3,d); rhs[0]-=cnst;
                a = -Cxx_i*x_i*y_i - Cxy_i*y_i*y_i + Cxy_i*x_i*x_i + Cyy_i*x_i*y_i;
                b = Cxx_i*y_i*y_i - Cxy_i*x_i*y_i - Cxy_i*x_i*y_i + Cyy_i*x_i*x_i;
                c = -Cxx_i*y_i + Cxy_i*x_i;
                d = -Cxy_i*y_i + Cyy_i*x_i;
                cnst = Cxx_i*mx_i*y_i + Cxy_i*my_i*y_i - Cxy_i*mx_i*x_i - Cyy_i*my_i*x_i;
                A.add(1, 0, a); A.add(1,1,b); A.add(1,2, c); A.add(1,3,d); rhs[1]-=cnst;
                a = Cxx_i*x_i + Cxy_i*y_i;
                b = -Cxx_i*y_i + Cxy_i*x_i;
                c = Cxx_i;
                d = Cxy_i;
                cnst = -Cxx_i*mx_i - Cxy_i*my_i;
                A.add(2, 0, a); A.add(2,1,b); A.add(2,2, c); A.add(2,3,d); rhs[2]-=cnst;
                a = Cxy_i*x_i + Cyy_i*y_i;
                b = -Cxy_i*y_i + Cyy_i*x_i;
                c = Cxy_i;
                d = Cyy_i;
                cnst = -Cxy_i*mx_i - Cyy_i*my_i;
                A.add(3, 0, a); A.add(3,1,b); A.add(3,2, c); A.add(3,3,d); rhs[3]-=cnst;
                k++;
                }
        BigMat V = new BigMat(4, 4);
        double[] w = new double[4];
        A.svdcmp(V, w);
        A.svbksb(w, V, rhs, rigidParams, 1e-4);
    }

    void solveLinear() {
        BigMat A = new BigMat(11,11);
        B = new BigMat(3, 11);
        for (int i=0; i<colHist.length; i++) {
            Gaussian g = colHist[i];
            double[] rgb = g.getMean();
            double[] poly = rgb2rgbpoly(rgb);
            double[] spec = spect[i];//rgbgrid[i];//
            for (int x=0; x<poly.length; x++) {
                for (int y=0; y<spec.length; y++) {
                    B.add(y,x,poly[x]*spec[y]);
                }
                for (int y=0; y<poly.length; y++) {
                    A.add(x,y,poly[x]*poly[y]);
                }
            }
        }
        BigMat V = new BigMat(11, 11);
        double[] w = new double[11];
        A.svdcmp(V, w);
        B = A.svbksb(w, V, B, 1e-4);
    }

    double[] rgb2rgbpoly(double[] rgb) {//, int type) {
        /*switch (type) {
            case 3: return rgb;
            case 5:
                double[] poly = {rgb[0], rgb[1], rgb[2], rgb[0]*rgb[1]*rgb[2], 1};
                return poly;
            case 6:
                double[] poly2 = {rgb[0], rgb[1], rgb[2], rgb[0]*rgb[1], rgb[0]*rgb[2], rgb};
                return poly2;
         * */
        double[] poly = {rgb[0], rgb[1], rgb[2], rgb[0]*rgb[1], rgb[0]*rgb[2], rgb[1]*rgb[2],
        rgb[0]*rgb[0], rgb[1]*rgb[1], rgb[2]*rgb[2], rgb[0]*rgb[1]*rgb[2], 1};
        return poly;

    }

    @Override
    public void finish() {
        //throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String getName() {
        return "Calibrate Colour";
    }

     static double finv(double t) {
        double split = (6.0/29.0);
        //split *= split*split;
        if (t>split) {
            return Math.pow(t, 3.0);
        }
        return (t-4.0/29.0)*(6.0/29.0)*(6.0/29.0)*3.0;
    }

    static double f(double t) {
        double split = (6.0/29.0);
        split *= split*split;
        if (t>split) {
            return Math.pow(t, 1.0/3.0);
        }
        return t*(29.0/6.0)*(29.0/6.0)/3.0 + 4.0/29.0;
    }

    /** Convert XYZ to L*a*b*
     *
     * @param col the XYZ colour
     * @param wp the whitepoint
     * @return the colour in L*a*b*
     */
    public static double[] XYZtoLab(double[] col, double[] wp) {
        double f0 = f(col[1]/wp[1]);
        double L = 116.0*f0-16.0; // index 0 = Y
        double a = 500*(f(col[0]/wp[0])-f0);
        double b = 200*(f0-f(col[2]/wp[2]));
        double[] res = {L, a, b};
        return res;
    }

    /** Convert  L*a*b* to  XYZ
     *
     * @param col the  L*a*b* colour
     * @param wp the whitepoint
     * @return the colour in XYZ
     */
    public static double[] LabtoXYZ(double[] col, double[] wp) {
        double f0 = (col[0]+16.0)/116.0;
        double Y = wp[1]*finv(f0); // index 0 = Y
        double X = wp[0]*finv(f0+col[1]/500.0);
        double Z = wp[2]*finv(f0-col[2]/200.0);
        double[] res = {X, Y, Z};
        return res;
    }


    /**
     * XYZ to sRGB conversion matrix
     */
    double[][] Mi  = {{ 3.2406, -1.5372, -0.4986},
                             {-0.9689,  1.8758,  0.0415},
                             { 0.0557, -0.2040,  1.0570}};


        /**
     * Convert XYZ to RGB.
     * @param X X
     * @param Y Y
     * @param Z Z
     * @return RGB in int array.
     */
    public int[] XYZtoRGB(double X, double Y, double Z) {
      int[] result = new int[3];

      double x = X / 100.0;
      double y = Y / 100.0;
      double z = Z / 100.0;

      // [r g b] = [X Y Z][Mi]
      double r = (x * Mi[0][0]) + (y * Mi[0][1]) + (z * Mi[0][2]);
      double g = (x * Mi[1][0]) + (y * Mi[1][1]) + (z * Mi[1][2]);
      double b = (x * Mi[2][0]) + (y * Mi[2][1]) + (z * Mi[2][2]);

      // assume sRGB
      if (r > 0.0031308) {
        r = ((1.055 * Math.pow(r, 1.0 / 2.4)) - 0.055);
      }
      else {
        r = (r * 12.92);
      }
      if (g > 0.0031308) {
        g = ((1.055 * Math.pow(g, 1.0 / 2.4)) - 0.055);
      }
      else {
        g = (g * 12.92);
      }
      if (b > 0.0031308) {
        b = ((1.055 * Math.pow(b, 1.0 / 2.4)) - 0.055);
      }
      else {
        b = (b * 12.92);
      }

      r = (r < 0) ? 0 : (r>1 ? 1 : r);
      g = (g < 0) ? 0 : (g>1 ? 1 : g);
      b = (b < 0) ? 0 : (b>1 ? 1 : b);

      // convert 0..1 into 0..255
      result[0] = (int) Math.round(r * 255);
      result[1] = (int) Math.round(g * 255);
      result[2] = (int) Math.round(b * 255);

      return result;
    }

    /**
     * Convert XYZ to RGB
     * @param XYZ in a double array.
     * @return RGB in int array.
     */
    public int[] XYZtoRGB(double[] XYZ) {
      return XYZtoRGB(XYZ[0], XYZ[1], XYZ[2]);
    }


    double[] getProb(BufferedImage bimg, Gaussian cHist, Ellipse2D rect) {
        double mx=0, my=0, mxx=0, myy=0, mxy=0, weight = 0;
        double[] vec = {0, 0, 0};
        for (int y=(int)(rect.getY()); y<(int)(rect.getY()+rect.getHeight()); y++)
            for (int x=(int)rect.getX(); x<(int)(rect.getX()+rect.getWidth()); x++)
            {
                if (rect.contains((int)x, (int)y)) {
                int c = bimg.getRGB(x, y);
                Color col = new Color(c);
                vec[0] = (double)col.getRed();
                vec[1] = (double)col.getGreen();
                vec[2] = (double)col.getBlue();
                double p = cHist.probability(vec);
                weight += p;
                mx += p*x; my += p*y;
                mxx+= p*x*x; myy += p*y*y; mxy += p*x*y;
                }
            }
        mx/=weight; my/=weight;
        mxx/=weight; mxx -= mx*mx;
        myy/=weight; myy -= my*my;
        mxy/=weight; mxy -= mx*my;

        double det = mxx * myy - mxy * mxy;
        det = det < 1 ? 1 : det;
        double w = 1 / (Math.PI * 2 * Math.sqrt(det));
        double MXX = myy / det, MYY = mxx / det, MXY = -mxy / det;

        double[] result = {mx, my, MXX, MYY, MXY};//mxx, myy, mxy};

        return result;
    }

    public boolean getReadTemplate() {
        return false;
    }

    public boolean getWriteTemplate() {
        return false;
    }

    public boolean getWriteImage() {
        return true;
    }

    
}
