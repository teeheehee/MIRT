unit ImageProcessingMethodsUnit;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Description:                                                               //
//  Container for Image Processing Methods variables                          //
//  Provides a way to pattern behavior for image resizing                     //
//                                                                            //
// Defaults:                                                                  //
//  FSourceMethod        - zsmDirectory (we're working with a root directory) //
//  FFileTypeMethod      - zftmPreserve (preserve file type after resize)     //
//  FScalingMethod       - zsmCalculate (calculate scale from height/width)   //
//  FFiletypeConversion  - zftcJPEG (if we convert filetypes, go to JPEG)     //
//  FFileHandling        - zfhSkip (skip over if a resized image exists)      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  Classes, ResizerCommonTypesUnit, ImageCommonTypesUnit;

type

  TImageProcessingMethods = class
  private
    FSourceMethod: TSourceMethod;
    FFiletypeMethod: TFiletypeMethod;
    FScalingMethod: TScalingMethod;
    FFiletypeConversion: TFiletypeConversion;
    FFileHandling: TFileHandling;
  public                        
    ////////////////////////////
    // Constructor/Destructor //
    ////////////////////////////
    constructor Create;
    destructor Destroy; reintroduce; overload;
  published
    property SourceMethod:        TSourceMethod       read FSourceMethod        write FSourceMethod;
    property FileTypeMethod:      TFiletypeMethod     read FFiletypeMethod      write FFiletypeMethod;
    property ScalingMethod:       TScalingMethod      read FScalingMethod       write FScalingMethod;
    property FiletypeConversion:  TFiletypeConversion read FFiletypeConversion  write FFiletypeConversion;
    property FileHandling:        TFileHandling       read FFileHandling        write FFileHandling;
  end; // TImageProcessingMethods


implementation

{ TImageProcessingMethods }

////////////////////////////
// Constructor/Destructor //
////////////////////////////
constructor TImageProcessingMethods.Create;
begin
  inherited;

  ///////////////////////////
  // Initialize everything //
  ///////////////////////////
  SourceMethod        := zsmDirectory;
  FileTypeMethod      := zftmPreserve;
  ScalingMethod       := zsmCalculate;
  FiletypeConversion  := zftcJPEG;
  FileHandling        := zfhSkip;
end;

destructor TImageProcessingMethods.Destroy;
begin           
  //////////////
  // Clean up //
  //////////////

  inherited;
end;

end.
 