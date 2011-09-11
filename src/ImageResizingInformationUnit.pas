unit ImageResizingInformationUnit;
///////////////////////////////////////////////////
//                                               //
// Description:                                  //
//  Container for image resizing information     //
//  Provides information on how to resize images //
//                                               //
// Defaults:                                     //
//  FFileName         - (blank)                  //
//  FWidth            - 800                      //
//  FHeight           - 600                      //
//  FCurrentWidth     - 0                        //
//  FCurrentHeight    - 0                        //
//  FConvertedWidth   - 0                        //
//  FConvertedHeight  - 0                        //
//  FScale            - 1                        //
//  FCompressMethod   - False                    //
//  FJPEGCompression  - 80                       //
//                                               //
///////////////////////////////////////////////////

interface

uses
  Classes;

type
  TImageResizingInformation = class
  private
    ///////////////////////
    // Image information //
    ///////////////////////
    FFileName: String;          // current image filename + path
    FWidth: integer;            // width to hold to
    FHeight: integer;           // height ot hold to
    FCurrentWidth: integer;     // current image width
    FCurrentHeight: integer;    // current image height
    FConvertedWidth: integer;   // current image converted width
    FConvertedHeight: integer;  // current image converted height
    FScale: real;               // scale to hold to
    FCompressMethod: Boolean;   // False: use image compression value
    FJPEGCompression: integer;  // JPEG Compression level setting
  public
    ////////////////////////////
    // Constructor/Destructor //
    ////////////////////////////
    constructor Create;
    destructor Destroy; reintroduce; overload;
  published
    property FileName:        String      read FFileName        write FFileName;
    property Width:           integer     read FWidth           write FWidth;
    property Height:          integer     read FHeight          write FHeight;
    property CurrentWidth:    integer     read FCurrentWidth    write FCurrentWidth;
    property CurrentHeight:   integer     read FCurrentHeight   write FCurrentHeight;
    property ConvertedWidth:  integer     read FConvertedWidth  write FConvertedWidth;
    property ConvertedHeight: integer     read FConvertedHeight write FConvertedHeight;
    property Scale:           real        read FScale           write FScale;
    property CompressMethod:  Boolean     read FCompressMethod  write FCompressMethod;
    property JPEGCompression: integer     read FJPEGCompression write FJPEGCompression;
  end; // TImageResizingInformation

implementation

{ TImageResizingInformation }

////////////////////////////
// Constructor/Destructor //
////////////////////////////
constructor TImageResizingInformation.Create;
begin
  inherited;

  ///////////////////////////
  // Initialize everything //
  ///////////////////////////
  FFileName               := '';
  FWidth                  := 800;
  FHeight                 := 600;
  FCurrentWidth           := 0;
  FCurrentHeight          := 0;
  FConvertedWidth         := 0;
  FConvertedHeight        := 0;
  FScale                  := 1;
  FCompressMethod         := False; // use image's compression value
  FJPEGCompression        := 80;    // arbitrary default value, decent quality
end;

destructor TImageResizingInformation.Destroy;
begin
  //////////////
  // Clean up //
  //////////////

  inherited;
end;

end.
