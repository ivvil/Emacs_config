package Herencia;

public class Series extends Video {
	private int startYear;
	private int endYear;
	private int numSeasons;
	private Season[] seasons;

	public Series(String title, String country, String genere, int rating, int startYear, int endYear, int numSeasons) {
		super(title, country, genere, rating);
		this.startYear = startYear;
		this.endYear = endYear;
		this.numSeasons = numSeasons;
		this.seasons = new Season[numSeasons];

	}

	@Override
	public String toString() {
		String seasonsString = "";
		if (seasons != null) {
			for (int i = 0; i < seasons.length; i++) {
				if (seasons[i] != null) {
					seasonsString += seasons[i].toString();
				}
			}
		}
		
		return String.format("%s, startYear=%s, endYear=%s, numSeasons=%d, seasons=[%s]", super.toString(), startYear, endYear, numSeasons,
							 seasonsString);
	}

	public int getStartYear() {
		return startYear;
	}
}
