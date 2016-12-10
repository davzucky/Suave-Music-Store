module SuaveMusicStore.App

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors

let html container =
    OK (View.index container)
    >=> Writers.setMimeType "text/html; charset=utf-8"

let details id =
    match Db.getAlbumDetails id with
    | Some album ->
        html (View.details album)
    | None ->
        never

let overview = warbler (fun _ ->
    Db.getGenres 
    |> List.map (fun g -> g.Name) 
    |> View.store 
    |> html)

let browse =
    request (fun r ->
        match r.queryParam Path.Store.browseKey with
        | Choice1Of2 genre -> 
            Db.getAlbumsForGenre genre
            |> View.browse genre
            |> html
        | Choice2Of2 msg -> BAD_REQUEST msg)

let manage = warbler (fun _ ->
    Db.getAlbumsDetails
    |> View.manage
    |> html)

let deleteAlbum id =
    
    match Db.getAlbum id  with
    | Some album ->
        choose [ 
            GET >=> warbler (fun _ -> 
                html (View.deleteAlbum album.Title))
            POST >=> warbler (fun _ -> 
                Db.deleteAlbum album ; 
                Redirection.FOUND Path.Admin.manage)
        ]
    | None ->
        never

let webPart = 
    choose [
        path Path.home >=> html View.home
        path Path.Store.overview >=> overview
        path Path.Store.browse >=> browse
        pathScan Path.Store.details details
        path Path.Admin.manage >=> manage
        pathScan Path.Admin.deleteAlbum deleteAlbum
        pathRegex "(.*)\.(css|png)" >=> Files.browseHome
        html View.notFound
    ]
    
startWebServer defaultConfig webPart