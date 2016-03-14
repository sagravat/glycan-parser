package util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by Sanjay Agravat on 6/1/15.
 */
public class GlycanInverter {

	public static String invert(String s) {

//		System.out.println("invert: " + s);
		List<String> residues = new ArrayList<String>();

		String first = "";
		String rest = s;
		if (s.startsWith("G") || (s.charAt(2) + "").matches("[A-Z]")) {
			first = s.charAt(0)+"";
			rest = s.substring(1);
		}
		int length = rest.length();
		boolean isParen = (rest.startsWith("(") || first.startsWith("("));
		//for (int i = 0; i < length;) {
		while (!rest.equals("")) {
			String residue = "";
			if (isParen) {

				//residue = rest.substring(0,5);
				String paren = rest.substring(0,1);
				residues.add(paren.equals(")") ? "(" : ")");

			}   else {

				residue = rest.substring(0,3);
				residues.add(residue);
			}
			//System.out.println(residue);
			if (isParen) {

				rest = rest.substring(1);
				isParen = false;
			} else {

				rest = rest.substring(3);

			}
			if (rest.startsWith("(") || rest.startsWith(")")) {
				isParen = true;
			} else {
			}

		}
		Collections.reverse(residues);
		StringBuilder sb = new StringBuilder();
		for (String residue : residues) {
			sb.append(residue);
		}
		sb.append(first);

		return sb.toString();


	}


}
