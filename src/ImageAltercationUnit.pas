unit ImageAltercationUnit;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Description: Routines for processing images and returning a specific type  //
//                                                                            //
//  ResizeImage                                                               //
//    The work horse. Takes an image and resizes it                           //
//                                                                            //
//  ResizeImageBMP                                                            //
//    Resizes image, passing out a bitmap                                     //
//                                                                            //
//  ResizeImageJPEG                                                           //
//    Resizes image, passing out a JPEG                                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface
                 
uses
  Windows, Graphics, JPEG;

function ResizeImage(imgOld: TGraphic; iHeight, iWidth: integer): TBitmap;
function ResizeImageBMP(imgOld: TGraphic; iHeight, iWidth: integer): TBitmap;
function ResizeImageJPEG(imgOld: TGraphic; iHeight, iWidth: integer): TJPEGImage;
///////////////////////////
// Not implemented yet : //
///////////////////////////
{
function ResizeImageGIF
function ResizeImagePNG
}

implementation

uses
  Classes, SysUtils;

function ResizeImage(imgOld: TGraphic; iHeight, iWidth: integer): TBitmap;
///////////////////////////////////////////////////////////////////
// Preconditions :                                               //
//  imgOld is a valid TGraphic                                   //
//  iHeight and iWidth are valid height and width integer values //
//                                                               //
// Output :                                                      //
//  TBitmap of imgOld, resized to iHeight and iWidth values      //
///////////////////////////////////////////////////////////////////
var
  imgBMP: TBitmap;
  rectProxyRect: TRect;
begin
  imgBMP := TBitmap.Create;
  
  try
    rectProxyRect := Rect(0, 0, iWidth, iHeight);

    with imgBMP do
    begin
      Height := iHeight;
      Width := iWidth;
      if Height = 0 then
        Height := 1;
      if Width = 0 then
        Width := 1;
      Canvas.StretchDraw(rectProxyRect, imgOld);
    end;

    Result := imgBMP;
  except on E:Exception do
    Raise Exception.Create('Error resizing image: ' + E.Message);
  end; // try..except
end;

function ResizeImageBMP(imgOld: TGraphic; iHeight, iWidth: integer): TBitmap;
///////////////////////////////////////////////////////////////////
// Preconditions :                                               //
//  imgOld is a valid TGraphic                                   //
//  iHeight and iWidth are valid height and width integer values //
//                                                               //
// Output :                                                      //
//  TBitmap of imgOld, resized to iHeight and iWidth values      //
///////////////////////////////////////////////////////////////////
begin
  Result := ResizeImage(imgOld, iHeight, iWidth);
end;

function ResizeImageJPEG(imgOld: TGraphic; iHeight, iWidth: integer): TJPEGImage;
///////////////////////////////////////////////////////////////////
// Preconditions :                                               //
//  imgOld is a valid TGraphic                                   //
//  iHeight and iWidth are valid height and width integer values //
//                                                               //
// Output :                                                      //
//  TJPEGImage of imgOld, resized to iHeight and iWidth values   //
///////////////////////////////////////////////////////////////////
var        
  imgBMP: TBitmap;
  imgJPEG: TJPEGImage;
begin
  imgJPEG := TJPEGImage.Create;
  imgBMP := ResizeImage(imgOld, iHeight, iWidth);
  
  if imgBMP <> nil then
  begin
    imgJPEG.Assign(imgBMP);
    Result := imgJPEG;
  end
  else
    Result := nil;

  FreeAndNil(imgBMP);
end;

end.

