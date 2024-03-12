package Herencia;



public class Movie extends Video {
	private int releaseYear;
	private int durationInMinutes;
	private String director;

	public Movie(String title, String country, String genere, int rating, int releaseYear, int durationInMinutes, String director) {
		super(title, country, genere, rating);
		this.releaseYear = releaseYear;
		this.durationInMinutes = durationInMinutes;
		this.director = director;
	}

	@Override
	public String toString() {
		return super.toString().substring(super.toString().length()) + String.format(" [ Movie [releaseYear=%d, durationInMinutes=%d, director=%s]]", releaseYear, durationInMinutes, director);
	}

	public String getDirector() {
		return director;
	}
}
