package Herencia;
import java.util.Comparator;

class OrderByMoviesThenSeries implements Comparator<Video> {

	@Override
	public int compare(Video v1, Video v2) {
		if (v1 instanceof Movie) {
			if (v2 instanceof Movie) {
				return 0;
			}
			return 1;
		}
		return -1;
	}
}
