unit ImageFilesUnit;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Description: Various routines that can be performed to help in processing  //
//  image files, or lists of files                                            //
//                                                                            //
//  ImageTypeSetToString                                                      //
//    Takes an ImageTypeSet and returns the string equivalent                 //
//                                                                            //
//  StringToImageFileType                                                     //
//    Takes a valid image file extension and returns the equivalent image     //
//    type                                                                    //
//                                                                            //
//  BuildImagesList                                                           //
//    Takes a list of directories and finds image files based on requested    //
//    type                                                                    //
//                                                                            //
//  RemoveQualifiedImages                                                     //
//    Will determine if files need to be in the list or not - removing based  //
//    on height and width requirements                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  Classes, SysUtils, ImageCommonTypesUnit, ResizerCommonTypesUnit;

function ImageTypeSetToString(itsImages: TImageTypeSet): String;

function BuildImagesList(const slDirs: TStringList;
  itsImages: TImageTypeSet): TStringList;
function CreateDestinationFilename(const sRoot, sDestination, sFileName,
  sExtension, sPrefix, sSuffix: String; zSourceMethod: TSourceMethod): String;

procedure RemoveQualifiedImages(var slFiles: TStringList; iWidth, iHeight: integer);

function IsGraphicsFile(const sValue: String): Boolean;

implementation

uses
  FindFilesUnit, ResizerConstsUnit, Graphics, JPEG;

function ImageTypeSetToString(itsImages: TImageTypeSet): String;
/////////////////////////////////////////////////////////////////////
// Preconditions :                                                 //
//  itsImages contains a set of image file types as defined above  //
//                                                                 //
// Output :                                                        //
//  String of image types, enclosed in parens                      //
//  e.g. : (bmp, jpg, gif, png)                                    //
/////////////////////////////////////////////////////////////////////
  function AddType(const sCurrent, sAddTo: String): String;
  //////////////////////////////////////////////////////////////////////
  // Preconditions :                                                  //
  //  sCurrent is a string of at least 1 character '('                //
  //                                                                  //
  // Output :                                                         //
  //  sCurrent with sAddTo concatenated, with a comma where necessary //
  //////////////////////////////////////////////////////////////////////
  begin
    if Length(sCurrent) > 1 then
      Result := sCurrent + ', ' + sAddTo
    else
      Result := sCurrent + sAddTo;
  end; // function AddType
var
  sTemp: String;
begin
  sTemp := '(';

  if ziftBMP in itsImages then
    AddType(sTemp, 'bmp');

  if ziftJPG in itsImages then
    AddType(sTemp, 'jpg');

  ///////////////////////////
  // Not implemented yet : //
  ///////////////////////////
  {
  if ziftGIF in itsImages then
    AddType(sTemp, 'gif');

  if ziftPNG in itsImages then
    AddType(sTemp, 'png');
  }

  sTemp := sTemp + ')';
end; // ImageTypeSetToString

function BuildImagesList(const slDirs: TStringList;
  itsImages: TImageTypeSet): TStringList;
////////////////////////////////////////////////////////////////////
// Preconditions :                                                //
//  slDirs is a valid list of directories                         //
//  itsImages contains a set of image file types as defined above //
//                                                                //
// Output :                                                       //
//  Sorted StringList of of files in slDirs directories matching  //
//  filetypes of itsImages                                        // 
//  Result StringList must be destroyed by owner                  //
////////////////////////////////////////////////////////////////////
  procedure AddFilesToList(var slCurrentList: TStringList;
    const slToAddList: TStringList);
  /////////////////////////////////////////////////////////
  // Preconditions :                                     //
  //  slCurrentList is initialized                       //
  //  slToAddList is initialized                         //
  //                                                     //
  // Output :                                            //
  //  slCurrentList has items in slToAddList added to it //
  /////////////////////////////////////////////////////////
  var
    i: integer;
  begin
    if slToAddList.Count > 0 then
      for i := 0 to slToAddList.Count - 1 do
        slCurrentList.Add(slToAddList.Strings[i]);
  end; // procedure AddFilesToList
var
  slResult: TStringList;
begin
  try
    slResult := TStringList.Create;

    if ziftBMP in itsImages then
      AddFilesToList(slResult, FindFiles(slDirs, EXTENSION_BMP));

    if ziftJPG in itsImages then
    begin
      AddFilesToList(slResult, FindFiles(slDirs, EXTENSION_JPG));
      AddFilesToList(slResult, FindFiles(slDirs, EXTENSION_JPEG));
    end; // if ziftJPG in itsImages

    ///////////////////////////
    // Not implemented yet : //
    ///////////////////////////
    {
    if ziftGIF in itsImages then
      AddFilesToList(slResult, FindFiles(slDirs, EXTENSION_GIF));

    if ziftPNG in itsImages then
      AddFilesToList(slResult, FindFiles(slDirs, EXTENSION_PNG));
    }

    slResult.Sort;

    Result := slResult;
  except on E:Exception do
    Raise Exception.Create('Failure to build images list '
      + ImageTypeSetToString(itsImages) + #10#13 + E.Message);
  end; // try..except
end; // BuildImagesList

function CreateDestinationFilename(const sRoot, sDestination, sFileName,
  sExtension, sPrefix, sSuffix: String; zSourceMethod: TSourceMethod): String;
//////////////////////////////////////////////////////////////////////////
// Preconditions :                                                      //
//  sFileName is the current image filename (+ path)                    //
//  sExtension is the extension to be used in the new filename (.jpg)   //
//  zSourceMethod needs to be valid                                     //
//  sRoot needs to be a valid location to be stripped from sFileName    //
//  sPrefix and FSuffix must be set to desired values                   //
//  sDestination needs to be a valid location                           //
//                                                                      //
// Output :                                                             //
//  Returns the complete filename of the output image                   //
//////////////////////////////////////////////////////////////////////////
var
  sTemp: String;
begin
  if (zSourceMethod = zsmRecursiveDirectory) then
  begin
    sTemp := Copy(sFileName,
      Length(IncludeTrailingBackslash(sRoot)) + 1,
      (Length(sFileName)
        - Length(IncludeTrailingBackslash(sRoot))
        - Length(ExtractFileName(sFileName))) - 1);

    Result := IncludeTrailingBackslash(sDestination)
      + IncludeTrailingBackslash(sTemp)
      + sPrefix
      + ChangeFileExt(ExtractFileName(sFileName), '') + sSuffix + sExtension;
  end // if (zSourceMethod = zsmRecursiveDirectory)
  else
  begin
    //////////////////////////////////////////////
    // FSourceMethod = zsmFiles or zsmDirectory //
    //////////////////////////////////////////////
    Result := IncludeTrailingBackslash(sDestination) + sPrefix
      + ChangeFileExt(ExtractFileName(sFileName), '') + sSuffix + sExtension;
  end; // if zSourceMethod = zsmFiles
end; // CreateDestinationFilename

procedure RemoveQualifiedImages(var slFiles: TStringList; iWidth, iHeight: integer);
//////////////////////////////////////////////////////////////////////////
// Preconditions :                                                      //
//  slFiles is a valid list of files                                    //
//  iWidth, iHeight are valid integers                                  //
//                                                                      //
// Output :                                                             //
//  Sorted StringList slFiles of revised files, missing thost that have //
//  dimensions within the range of iWidth and iHeight                   //
//////////////////////////////////////////////////////////////////////////
var
  i: integer;
  bGood: Boolean;
  imgTemp: TGraphic;
begin
  if slFiles.Count > 0 then
  begin
    for i := slFiles.Count - 1 downto 0 do
    begin
      bGood := False;

      try
        ////////////////
        // Load image //
        ////////////////
        case FileNameToImageFileType(slFiles.Strings[i]) of
          ziftJPG:
            imgTemp := TJPEGImage.Create;
          ziftBMP:
            imgTemp := TBitmap.Create;
          //////////////////////////
          // Not implemented yet: //
          //////////////////////////
          {
          ziftGIF:
            imgTemp := TGIFImage.Create;
          ziftPNG:
            imgTemp := TPNGImage.Create;
          }
        end; // case FileNameToImageFileType(slFiles.Strings[i])

        if Assigned(imgTemp) then
        begin
          ////////////////////////
          // Compare dimensions //
          ////////////////////////
          imgTemp.LoadFromFile(slFiles.Strings[i]);
          bGood := (iWidth <= imgTemp.Width) and (iHeight <= imgTemp.Height);
        end;
      finally
        FreeAndNil(imgTemp);
      end; // try..finally

      //////////////////////////////////////////
      // Remove from string list if necessary //
      //////////////////////////////////////////
      if bGood then
        slFiles.Delete(i);
    end; // for i := slFilesCount - 1 downto 0
  end; // if slFiles.Count > 0

  slFiles.Sort;
end; // RemoveQualifiedImages

function IsGraphicsFile(const sValue: String): Boolean;
//////////////////////////////////////////////////////////////////////////////
// Preconditions :                                                          //
//  sValue is a String, holding either a file or directory path             //
//    (as from a drag-drop operation)                                       //
//                                                                          //
// Output :                                                                 //
//  True if the sValue given is a file with a recognized graphics extension //
//  False if otherwise                                                      //
//////////////////////////////////////////////////////////////////////////////
var
  bTemp: Boolean;
begin
  bTemp := False;

  if not DirectoryExists(sValue) then
  begin
    case FileNameToImageFileType(sValue) of
      ziftBMP,
      ziftJPG:
        bTemp := True;
      //////////////////////////
      // Not implemented yet: //
      //////////////////////////
      {
      ziftGIF,
      ziftPNG:
      }
    end; // case FileNameToImageFileType(sValue)
  end; // if not DirectoryExists(sValue)

  Result := bTemp;
end; // IsGraphicsFile


end.

