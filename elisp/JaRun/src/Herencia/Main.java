package Herencia;

import java.util.Arrays;
import java.util.Random;

public class Main {
	
	public static void main(String[] args) {
		// Crear catalogo

		Catalogue catalogue = new Catalogue(10);

		// Insertar una película/serie en el catálogo.

		Movie movie1 = new Movie("Sexo En Nueva York 25", "Gringolandia", "Nueva York", 3, 2075, -3, "David Lynch");
		Movie movie2 = new Movie("Sexo En Nueva York 2", "Gringolandia", "Nueva York", 3, 2099, -3, "David Lynch");
		Movie movie3 = new Movie("Resacon en las vegas: menos vegas", "Gringolandia", "Gringos borrachos", 2, 2045, -3, "JJ Abrams");

		Series series1 = new Series("Los simpson", "Gringolandia", "Slop", 2, 1989, 2099, 110);
		Series series2 = new Series("Juego de tronos", "Gringolandia", "Tronos", 4, 2011, 2019, 8);


		catalogue.add(movie1);
		catalogue.add(movie2);
		catalogue.add(movie3);
		catalogue.add(genRandomMovie());
		catalogue.add(genRandomMovie());
		catalogue.add(series1);
		catalogue.add(series2);
		catalogue.add(genRandomMovie());
		catalogue.add(genRandomMovie());
		catalogue.add(genRandomSeries());
		catalogue.add(genRandomSeries());

		// Consultar todo el catálogo.
		
		System.out.println("-- Consultar el catalogo --");
		System.out.println(catalogue.toString());
		System.out.println();


		// Consultar las películas de un director determinado.

		System.out.println("-- Consultar por director --");
		Video[] director = catalogue.consultByDirector("David Lynch");
		for (int i = 0; i < director.length; i++) {
			System.out.println(director[i]);
		}
		System.out.println();
		
		// Consultar las series por año de inicio de emisión

		System.out.println("-- Consultar por inicio de emision --");
		Video[] startYear = catalogue.consultByDirector("David Lynch");
		for (int i = 0; i < startYear.length; i++) {
			System.out.println(startYear[i]);
		}
		System.out.println();

		// Consultar todos los videos de un determinado género.

		System.out.println("-- Consultar por género --");
		Video[] genere = catalogue.consultByGenere("Nueva York");
		for (int i = 0; i < genere.length; i++) {
			System.out.println(genere[i]);
		}
		System.out.println();

		// Modificar el género de un video por código.
		
		System.out.println("-- Modificar por codigo --");
		catalogue.modifyGenereByID(1, "Sexo");
		System.out.println(catalogue.toString());
		System.out.println();
		
		// Eliminar una película/serie por código.

		System.out.println("-- Borrar por código --");
		catalogue.deleteByID(1);
		System.out.println(catalogue.toString());
		System.out.println();
		
		// Indicar el número de episodios de una temporada de una serie determinada.
		// TODO: Fix this one
		// catalogue.setNumEpisodes(null);
		
		// Indicar los episodios totales (de todas las temporadas) de una serie determinada.

		// TODO: Make this one
		
		// Ordenar el catálogo de forma que primero estén las películas y luego las series.

		System.out.println("-- Ordenar por películas, luego series --");
		Arrays.sort(catalogue.videos, new OrderByMoviesThenSeries());
		System.out.println(catalogue);
		System.out.println();

		// Ordenar el catálogo por título.

		System.out.println("-- Ordenar por titulo --");
		Arrays.sort(catalogue.videos, new OrderByTitle());
		System.out.println(catalogue);
		System.out.println();
		
		// Ordenar el catálogo por género.

		System.out.println("-- Ordenar por género --");
		Arrays.sort(catalogue.videos, new OrderByGenere());
		System.out.println(catalogue);
		System.out.println();
		
		// Ordenar el catálogo por puntuación y después por título.

		System.out.println("-- Ordenar por puntuación, luego título --");
		Arrays.sort(catalogue.videos, new OrderByRatingThenTitle());
		System.out.println(catalogue);
		System.out.println();
		
		// Consultar los 10 videos con mejor puntuación.
		
		System.out.println("-- Consultar los 10 videos con mejor puntuación --");
		Video[] rating = catalogue.consultHighestRating();
		for (int i = 0; i < rating.length ; i++) {
			System.out.println(rating[i]);
		}
		System.out.println();
		
		// Dados dos catalógos consultar qué catálogo tiene más series almacenadas.

		Catalogue catalogue2 = new Catalogue(2);

		catalogue2.add(series1);
		catalogue2.add(series2);

		System.out.println("-- Consultar catalogo con mejor puntuación --");
		System.out.println(Catalogue.biggerBySeries(catalogue, catalogue2) != catalogue ? "Primer catalogo" : "Segundo catalogo");
		System.out.println();

	}

	public static Movie genRandomMovie() {
        Random rand = new Random();

        // Random strings for demonstration purposes, replace with your logic
        String title = genRandomString(10);
        String country = genRandomString(8);
        String genre = genRandomString(6);
        int rating = rand.nextInt(11); // Assuming rating is between 0 and 10
        int releaseYear = rand.nextInt(100) + 1920; // Assuming release year between 1920 and 2020
        int durationInMinutes = rand.nextInt(181); // Assuming duration between 0 and 180 minutes
        String director = genRandomString(12);

        return new Movie(title, country, genre, rating, releaseYear, durationInMinutes, director);
    }

	public static Series genRandomSeries() {
        Random rand = new Random();

        // Random strings for demonstration purposes, replace with your logic
        String title = genRandomString(10);
        String country = genRandomString(8);
        String genre = genRandomString(6);
        int rating = rand.nextInt(11); // Assuming rating is between 0 and 10
        int startYear = rand.nextInt(100) + 1920; // Assuming start year between 1920 and 2020
        int endYear = startYear + rand.nextInt(20); // Assuming end year is within 20 years from start year
        int numSeasons = rand.nextInt(10) + 1; // Assuming at least 1 season

        return new Series(title, country, genre, rating, startYear, endYear, numSeasons);
    }


	private static String genRandomString(int length) {
        Random rand = new Random();
        StringBuilder randomString = new StringBuilder();

        String characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

        for (int i = 0; i < length; i++) {
            char randomChar = characters.charAt(rand.nextInt(characters.length()));
            randomString.append(randomChar);
        }

        return randomString.toString();
    }
}

