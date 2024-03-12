package Herencia;

public class Episode extends Video {
	private String epTitle;
	private String description;

	public Episode(String title, String country, String genere, int rating, String epTitle, String description) {
		super(title, country, genere, rating);
		this.epTitle = epTitle;
		this.description = description;

	}

	@Override
	public String toString() {
		return String.format("Episode [title=%s, description=%s]", epTitle, description);
	}
}
