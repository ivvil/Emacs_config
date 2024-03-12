package Herencia;

import java.util.Comparator;

class OrderByGenere implements Comparator<Video> {

	@Override
	public int compare(Video v1, Video v2) {
		if (v1 == null || v2 == null) {
			if (v1 == v2) {
				return 0;
			} else { 
				return v1 == null ? 1 : -1;
			}
		} else {
			return v1.getGenere().compareTo(v2.getGenere());
		}
	}
}
