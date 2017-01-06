module SuaveMusicStore.Db

open FSharp.Data
open System.IO

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors

type Genres= CsvProvider<"./data/genres.csv">
type Artists= CsvProvider<"./data/artists.csv">
type Albums = CsvProvider<"./data/albums.csv">

let albumsFilePath = "./data/albums.csv"
let genres = Genres.Load("./data/genres.csv")
let artists = Artists.Load("./data/artists.csv")
let mutable albums = Albums.Load(albumsFilePath)

type AlbumDetails = { AlbumId:int; AlbumArtUrl:string; Price:decimal; Title:string; Artist:string; Genre:string } 

type Genre = Genres.Row
type Artist = Artists.Row
type Album = Albums.Row


let firstOrNone s = s |> Seq.tryFind (fun _ -> true)

let getGenres : Genre list = 
    genres.Rows |> Seq.toList

let getAlbumsForGenre genreName : Album list = 
    let genre = genres.Rows 
                    |> Seq.filter  (fun g -> g.Name = genreName)
                    |> Seq.exactlyOne
    albums.Rows 
        |> Seq.filter (fun a -> a.GenreId = genre.GenreId)
        |> Seq.toList

let getGenreName genreId = 
    genres.Rows
    |> Seq.filter (fun g -> g.GenreId = genreId)
    |> Seq.map (fun g -> g.Name)
    |> Seq.exactlyOne

let getArtistName artistId = 
    artists.Rows
    |> Seq.filter (fun a -> a.ArtistId = artistId)
    |> Seq.map (fun a -> a.Name)
    |> Seq.exactlyOne

let getAlbumDetails id : AlbumDetails option = 

    let albumDetail = albums.Rows 
                    |> Seq.filter  (fun a -> a.AlbumId = id)
                    |> Seq.exactlyOne
    Some { 
        AlbumId=albumDetail.AlbumId; 
        AlbumArtUrl=albumDetail.AlbumArtUrl; 
        Price=albumDetail.Price; 
        Title=albumDetail.Title; 
        Artist= getArtistName albumDetail.ArtistId; 
        Genre= getGenreName albumDetail.GenreId }

let getAlbumsDetails () : AlbumDetails list = 
    albums.Rows 
    |> Seq.map (fun a -> 
            { 
                AlbumId=a.AlbumId; 
                AlbumArtUrl=a.AlbumArtUrl; 
                Price=a.Price; 
                Title=a.Title; 
                Artist= getArtistName a.ArtistId; 
                Genre= getGenreName a.GenreId })
    |> Seq.toList

let getAlbum id : Album option = 
    albums.Rows
    |> Seq.tryFind(fun a -> a.AlbumId = id)
let deleteAlbum (album : Album) = 
    let albumsWithoutDelteItem = albums.Filter (fun a -> a.AlbumId <> album.AlbumId)
    use writer = new StreamWriter(albumsFilePath)
    let csvContent = albumsWithoutDelteItem.SaveToString()
    writer.Write(csvContent)
    writer.Flush() 
    writer.Close()
    albums <- Albums.Load(albumsFilePath)    

let getArtists () : Artist list = 
    artists.Rows |> Seq.toList
let getNextAlbumId =
    let maxId = 
        albums.Rows
        |> Seq.map (fun a -> a.AlbumId)
        |> Seq.max
    maxId + 1

let createAlbum (artistId, genreId, price, title) =
    let newRow = Albums.Row(getNextAlbumId, genreId, artistId, title, price, "/placeholder.gif")
    let newAlbumns = albums.Append [newRow]
    use writer = new StreamWriter(albumsFilePath)
    let csvContent = newAlbumns.SaveToString()
    writer.Write(csvContent)
    writer.Flush() 
    writer.Close()
    albums <- Albums.Load(albumsFilePath)  


