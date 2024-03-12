package Herencia;

public abstract class Video {
	private static int maxID = 0;
	private int ID;
	private String title;
	private String country;
	private String genere;
	private int rating;
	
    protected Video(String title, String country, String genere, int rating) {
		maxID++;
		this.ID = maxID;
		this.title = title;
		this.country = country;
		this.genere = genere;
		this.rating = rating;
	}

	@Override
	public String toString() {
		return String.format("Video [ID=%d, title=%s, country=%s, genere=%s, rating=%d]", ID, title, country, genere, rating);
	}

	public String getGenere() {
		return genere;
	}

	public int getID() {
		return ID;
	}

	public void setID(int ID) {
		this.ID = ID;
	 }

	public String getTitle() {
		return title;
	}

	public int getRating() {
		return rating;
	}

	public void setGenere(String genere) {
		this.genere = genere;

	}
}
