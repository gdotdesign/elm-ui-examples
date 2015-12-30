module Main where

import Html exposing (text)

import Entities.Association as Association

videos = [{ id = "0", name = "Video 01", folderId = "" }]
folders =
  [ { id = "1", name = "Folder 01", folderId = "" }
  , { id = "2", name = "Folder 02", folderId = "1" }
  ]

repository =
  { folders = folders
  , foldersAndVideos = Association.init .folderId videos folders
  , foldersAndFolders = Association.init .folderId folders folders
  }

main = text (toString folder)
