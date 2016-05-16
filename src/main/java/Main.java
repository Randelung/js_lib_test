public class Main {
    public static void main(String[] args) throws Jampack.JampackException {
        //Jampack.Zmat matrix = Rand.uzmat(3, 3);
        Jampack.Zmat matrix = new Jampack.Zmat(new double[][]{{1, 3, -3}, {-3, 7, -3}, {-6, 6, -2}});
        //Jampack.Zmat vector = new Jampack.Zmat(new double[][]{{1}, {1}, {1}});
        JampackNew.Zmat zmat = new JampackNew.Zmat(matrix.getRe(), matrix.getIm());
        //JampackNew.Zmat vect = new JampackNew.Zmat(vector.getRe(), vector.getIm());

        /*Jampack.Solve.aib(matrix, vector);
        zmat.solve(vect);

        Jampack.Eig eig = new Jampack.Eig(matrix);
        Jampack.Print.o(eig.X);
        System.out.println();

        JampackNew.Eig eig1 = zmat.eig();
        System.out.println(eig1.X());

        Jampack.Zhqrd qr = new Jampack.Zhqrd(matrix);
        JampackNew.Zhqrd qr1 = zmat.qr();
        Jampack.Print.o(qr.R);
        System.out.println(qr1.R());
        for (Jampack.Z1 value : qr.U) {
            Jampack.Print.o(value);
        }
        System.out.println(Arrays.deepToString(qr1.U()));

        Jampack.Zsvd svd1 = new Jampack.Zsvd(matrix);
        Jampack.Print.o(svd1.S);
        Jampack.Print.o(svd1.U);
        Jampack.Print.o(svd1.V);

        JampackNew.Zsvd svd2 = new JampackNew.Zsvd(zmat);
        System.out.println(svd2.S().toString());
        System.out.println(svd2.U().toString());
        System.out.println(svd2.V().toString());*/
    }
}
