unit ImageCommonTypesUnit;
//////////////////////////////////////////////////////////////////
//                                                              //
// Description: Types, enumerated or otherwise, useful to Image //
//              Conversion functions for stated types           //
//                                                              //
//////////////////////////////////////////////////////////////////

interface

type
  TImageFileType  = (ziftBMP, ziftPNG, ziftJPG, ziftGIF, ziftNone);
  TImageTypeSet   = set of TImageFileType;

function ExtensionToImageFileType(const sExtension: String): TImageFileType;
function StringToImageFileType(const sValue: String): TImageFileType;
function FileNameToImageFileType(const sFileName: String): TImageFileType;

function ImageFileTypeToString(iftValue: TImageFileType): String;
function ImageFileTypeToExtension(iftValue: TImageFileType): String;

implementation

uses
  Classes, SysUtils, ResizerConstsUnit;

function ExtensionToImageFileType(const sExtension: String): TImageFileType;
///////////////////////////////////////////////////////
// Preconditions :                                   //
//  sExtension is a valid file extension             //
//                                                   //
// Output :                                          //
//  Corresponding image file type based on extension //
//  If sExtension isn't recognized, returns ziftNone //
///////////////////////////////////////////////////////
var
  iftTemp: TImageFileType;
  sLowerExtension: String;
begin
  iftTemp := ziftNone;
  sLowerExtension := LowerCase(sExtension);

  if (CompareText(sLowerExtension, EXTENSION_JPG) = 0)
    or (CompareText(sLowerExtension, EXTENSION_JPEG) = 0) then
    iftTemp := ziftJPG
  else
  if CompareText(sLowerExtension, EXTENSION_BMP) = 0 then
    iftTemp := ziftBMP;
  ///////////////////////////
  // Not implemented yet : //
  ///////////////////////////
  {
  else
  if CompareText(sLowerExtension, EXTENSION_GIF) = 0 then
    Result := ziftGIF
  else
  if CompareTExt(sLowerExtension, EXTENSION_PNG) = 0 then
    Result := ziftPNG;
  }

  Result := iftTemp;
end; // ExtensionToImageFileType

function StringToImageFileType(const sValue: String): TImageFileType;
var                       
  iftTemp: TImageFileType;
  sUpperValue: String;
begin
  iftTemp := ziftNone;
  sUpperValue := UpperCase(sValue);

  if CompareText(sUpperValue, COMPARE_BMP) = 0 then
    iftTemp := ziftBMP
  else
  if (CompareText(sUpperValue, COMPARE_JPG) = 0)
    or (CompareText(sUpperValue, COMPARE_JPEG) = 0) then
    iftTemp := ziftJPG;
  //////////////////////////
  // Not implemented yet: //
  //////////////////////////
  {
  else
  if CompareText(sUpperValue, COMPARE_GIF) = 0 then
    iftTemp := ziftGIF
  else
  if CompareText(sUpperValue, COMPARE_PNG) = 0 then
    iftTemp := ziftPNG;
  }

  Result := iftTemp;
end; // StringToImageFileType

function FileNameToImageFileType(const sFileName: String): TImageFileType;
///////////////////////////////////////////////////////
// Preconditions :                                   //
//  sFileName is a valid filename                    //
//                                                   //
// Output :                                          //
//  Corresponding image file type based on extension //
//  If extension isn't recognized, returns ziftNone  //
///////////////////////////////////////////////////////
begin
  Result := ExtensionToImageFileType(ExtractFileExt(sFileName));
end; // FileNameToImageFileType

function ImageFileTypeToString(iftValue: TImageFileType): String;
var
  sTemp: String;
begin
  sTemp := '';

  case iftValue of
    ziftBMP:
      sTemp := COMPARE_BMP;
    ziftJPG:
      sTemp := COMPARE_JPG;
    //////////////////////////
    // Not implemented yet: //
    //////////////////////////
    {
    ziftGIF:
      sTemp := COMPARE_GIF;
    ziftPNG:
      sTemp := COMPARE_PNG;
    }
  end; // case iftValue

  Result := sTemp;
end; // ImageFileTypeToString

function ImageFileTypeToExtension(iftValue: TImageFileType): String;
var
  sTemp: String;
begin
  sTemp := '';

  case iftValue of
    ziftBMP:
      sTemp := EXTENSION_BMP;
    ziftJPG:
      sTemp := EXTENSION_JPG;
    //////////////////////////
    // Not implemented yet: //
    //////////////////////////
    {
    ziftGIF:
      sTemp := EXTENSION_GIF;
    ziftPNG:
      sTemp := EXTENSION_PNG;
    }
  end; // case iftValue

  Result := sTemp;
end; // ImageFileTypeToExtension

end.
