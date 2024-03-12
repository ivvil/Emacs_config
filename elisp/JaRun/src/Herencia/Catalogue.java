package Herencia;

import java.util.Arrays;

public class Catalogue {
    public Video[] videos;

	final static int NUM_OF_VIDEOS = 10;

	public Catalogue(int size) {
		videos = new Video[size];


	}

	public boolean add(Video video) {
		for (int i = 0; i < videos.length; i++) {
			if (videos[i] == null) {
				videos[i] = video;
				return true;
			}
		}
		return false;
	 }

	public Video[] consultByDirector(String director) {
		int arrSize = 0;
		
		for (int i = 0; i < videos.length; i++) {
			if (videos[i] instanceof Movie && ((Movie) videos[i]).getDirector().equals(director)){
				arrSize++;
			}
		}

		Video[] matches = new Video[arrSize];

		for (int i = 0; i < videos.length; i++) {
			if (videos[i] instanceof Movie && ((Movie) videos[i]).getDirector().equals(director) && videos[i] != null) {
				for (int j = 0; j < matches.length; j++) {
					if (matches[j] == null) {
						matches[j] = videos[i];
						break;
					}
				}
			}
		}

		return matches;

	}

	public Video[] consultByStartYear(int startYear) {
		int arrSize = 0;

		for (int i = 0; i < videos.length; i++) {
			if (videos[i] instanceof Series && ((Series) videos[i]).getStartYear() == startYear) {
				arrSize++;
			}
		}

	    Video[] matches = new Video[arrSize];

		for (int i = 0; i < videos.length; i++) {
			if (videos[i] instanceof Series && ((Series) videos[i]).getStartYear() == startYear) {
				for (int j = 0; j < matches.length; j++) {
					if (matches[j] == null) {
						matches[j] = videos[i];
						break;
					}
				}
			}
		}

		return matches;

	 }

	public Video[] consultByGenere(String genere) {
		int arrSize = 0;
		
		for (int i = 0; i < videos.length; i++) {
			if (videos[i].getGenere().equals(genere)){
				arrSize++;
			}
		}

		Video[] matches = new Video[arrSize];

		for (int i = 0; i < videos.length; i++) {
			if (videos[i].getGenere().equals(genere)) {
				for (int j = 0; j < matches.length; j++) {
					if (matches[j] == null) {
						matches[j] = videos[i];
						break;
					}
				}
			}
		}

		return matches;		 
	 }

	public Video[] consultHighestRating() {
		int size;
		
		if (NUM_OF_VIDEOS > videos.length) {
			size = videos.length;
		} else {
			size = NUM_OF_VIDEOS;
		}

		Catalogue sorted = this;
		sorted.sortByRatingThenTitle();

		return Arrays.copyOf(sorted.videos, size);
	 }

	public boolean modifyGenereByID(int ID, String genere) {
		for (int i = 0; i < videos.length; i++) {
			if (videos[i].getID() == ID) {
				videos[i].setGenere(genere);
				return true;
			}
		}
		return false;
	 }

	public boolean deleteByID(int ID) {
		 for (int i = 0; i < videos.length; i++) {
			 if (videos[i].getID() == ID) {
				 for (int j = i; j < videos.length - 1; j++) {
					 videos[j] = videos[j + 1];
				 }
				 videos[videos.length - 1] = null;
				 return true;
			 }
		 }

		 return false;
	 }

	public void setNumEpisodes(int ID, Season season) {
		 for (int i = 0; i < videos.length; i++) {
			 if (videos[i].getID() == ID && videos[i] instanceof Series) {
				 // videos[i].
			 }
		 }
	 }

	public void sortByMoviesThenSeries() {
		Arrays.sort(this.videos, new OrderByMoviesThenSeries());
	 }

	public void sortByTitle() {
		Arrays.sort(this.videos, new OrderByTitle());
	 }

	public void sortByGenere() {
		 Arrays.sort(this.videos, new OrderByGenere());
	 }

	public void sortByRatingThenTitle() {
		 Arrays.sort(this.videos, new OrderByRatingThenTitle());
	 }

	public int getNumSeries() {
		int series = 0;

		for (int i = 0; i < videos.length; i++) {
			if (videos[i] instanceof Series) {
				series++;
			}
		}

		return series;

	}

	public static Catalogue biggerBySeries(Catalogue c1, Catalogue c2) {
		return c1.getNumSeries() < c2.getNumSeries() ? c1 : c2;
	 }

	@Override
	public String toString() {
		String string = "";

		for (int i = 0; i < videos.length; i++) {
			if (videos[i] != null) {
				string += (videos[i].toString() + "\n");
			}
		}

		return string;
	 }

	
}
