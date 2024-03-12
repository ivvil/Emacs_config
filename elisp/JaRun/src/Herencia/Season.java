package Herencia;

class Season {
	private int numEpisodes;
	private Episode[] episodes;

	public Season(int numEpisodes) {
		this.numEpisodes = numEpisodes;
		this.episodes = new Episode[numEpisodes];

	}

	@Override
	public String toString() {
		String episodesString = "";
		if (episodes != null) {
			for (int i = 0; i < episodes.length; i++) {
				if (episodes[i] != null) {
					episodesString += episodes[i].toString();
				}
			}

			return episodesString;
		} else {
			return "";
		}
	}

	public int getNumEpisodes() {
		return numEpisodes;
	}

	public void setNumEpisodes(int numEpisodes) {
		this.numEpisodes = numEpisodes;

	}
}
