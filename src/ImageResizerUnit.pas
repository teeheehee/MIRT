unit ImageResizerUnit;

interface

uses
  Classes, ResizerCommonTypesUnit, ImageCommonTypesUnit,
  ImageProcessingMethodsUnit, FileStructureInformationUnit,
  ImageResizingInformationUnit;

type

  TImageResizer = class
  private
    FImageProcessingMethods: TImageProcessingMethods; // Image processing methods
    FImageInformation: TImageResizingInformation; // Image resizing information
    FFileInformation: TFileStructureInformation; // File structure information

    FContinueProcess: Boolean;  // if false, stop processing
    FBadFiles: TStringList;     // List of bad files found during resizing
    FDirListCurrent: Boolean;   // FFileInformation.Dirs has been compiled with current FFileInformation.Root
    FFilesListCurrent: Boolean; // FFileInformation.Files has been compiled or set with recent data

    /////////////////////////
    // Notification events //
    /////////////////////////
    FScaleCalculated: TNotifyEvent;
    FWidthCalculated: TNotifyEvent;
    FHeightCalculated: TNotifyEvent;
    FResizingStarted: TNotifyEvent;
    FResizingEnded: TNotifyEvent;
    FDestinationSet: TNotifyEvent;
    FLoadingFile: TNotifyEvent;
    FSavingFile: TNotifyEvent;

    /////////////
    // Getters //
    /////////////
    // Image Processing Methods
    function GetSourceMethod: TSourceMethod;
    function GetFiletypeMethod: TFiletypeMethod;
    function GetScalingMethod: TScalingMethod;
    function GetFiletypeConversion: TFiletypeConversion;
    function GetFileHandling: TFileHandling;
    // File Information
    function GetRoot: String;
    function GetDirs: TStringList;
    function GetFiles: TStringList;
    function GetOutputPath: String;
    function GetPrefix: String;
    function GetSuffix: String;
    function GetDestination: String;
    function GetImageFileType: TImageFileType;
    function GetImageTypeSet: TImageTypeSet;
    // Image Resizing Information
    function GetFileName: String;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetCurrentWidth: integer;
    function GetCurrentHeight: integer;
    function GetConvertedHeight: integer;
    function GetConvertedWidth: integer;
    function GetScale: real;
    function GetCompressMethod: Boolean;
    function GetJPEGCompression: integer;

    /////////////
    // Setters //
    /////////////
    // Image Processing Methods
    procedure SetSourceMethod(const Value: TSourceMethod);
    procedure SetFiletypeMethod(const Value: TFiletypeMethod);
    procedure SetScalingMethod(const Value: TScalingMethod);
    procedure SetFiletypeConversion(const Value: TFiletypeConversion);
    procedure SetFileHandling(const Value: TFileHandling);
    // File Information
    procedure SetRoot(const Value: String);
    procedure SetDirs(const Value: TStringList);
    procedure SetFiles(const Value: TStringList);
    procedure SetOutputPath(const Value: String);
    procedure SetPrefix(const Value: String);
    procedure SetSuffix(const Value: String);
    procedure SetDestination(const Value: String);
    procedure SetImageFileType(const Value: TImageFileType);
    procedure SetImageTypeSet(const Value: TImageTypeSet);
    // Image Resizing Information
    procedure SetFileName(const sFileName: String);
    procedure SetWidth(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetScale(fScaleValue: real);
    procedure SetCompressMethod(const Value: Boolean);
    procedure SetJPEGCompression(const Value: integer);
    // Calculations
    function CalculateScale(const iWidth, iHeight: integer): real;
    function CalculateWidth(const iWidth: integer): integer;
    function CalculateHeight(const iHeight: integer): integer;
    // Business logic
    procedure SetCurrentWidth(const iCurrentWidth: integer);
    procedure SetCurrentHeight(const iCurrentHeight: integer);
    procedure SetConvertedWidth(const iConvertedWidth: integer);
    procedure SetConvertedHeight(const iConvertedHeight: integer);
    procedure LogBadFile(const sFileName: String);
    // Main methods
    procedure CalculateImageDimensions;
  public
    constructor Create;
    destructor Destroy; reintroduce; overload;

    procedure ClearFilesList;

    procedure PrepareDirsList;
    procedure PrepareFilesList;
    // procedure ResizeSingle[index: integer];
    procedure ResizeAll;
  published
    // Image Processing Methods
    property SourceMethod:        TSourceMethod       read GetSourceMethod        write SetSourceMethod;
    property FileTypeMethod:      TFiletypeMethod     read GetFiletypeMethod      write SetFiletypeMethod;
    property ScalingMethod:       TScalingMethod      read GetScalingMethod       write SetScalingMethod;
    property FiletypeConversion:  TFiletypeConversion read GetFiletypeConversion  write SetFiletypeConversion;
    property FileHandling:        TFileHandling       read GetFileHandling        write SetFileHandling;

    property ContinueProcess:     Boolean             read FContinueProcess       write FContinueProcess;

    // File Information
    property Root:                String              read GetRoot          write SetRoot;
    property Dirs:                TStringList         read GetDirs          write SetDirs;
    property Files:               TStringList         read GetFiles         write SetFiles;
    property OutputPath:          String              read GetOutputPath    write SetOutputPath;
    property Prefix:              String              read GetPrefix        write SetPrefix;
    property Suffix:              String              read GetSuffix        write SetSuffix;
    property Destination:         String              read GetDestination   write SetDestination;
    property ImageFileType:       TImageFileType      read GetImageFileType write SetImageFileType;
    property ImageTypeSet:        TImageTypeSet       read GetImageTypeSet  write SetImageTypeSet;

    // Image Resizing Information
    property FileName:            String              read GetFileName;
    property Width:               integer             read GetWidth               write SetWidth;
    property Height:              integer             read GetHeight              write SetHeight;
    property CurrentWidth:        integer             read GetCurrentWidth;
    property CurrentHeight:       integer             read GetCurrentHeight;
    property ConvertedWidth:      integer             read GetConvertedWidth;
    property ConvertedHeight:     integer             read GetConvertedHeight;
    property Scale:               real                read GetScale               write SetScale;
    property CompressMethod:      Boolean             read GetCompressMethod      write SetCompressMethod;
    property JPEGCompression:     integer             read GetJPEGCompression     write SetJPEGCompression;

    property BadFiles:            TStringList         read FBadFiles;

    // Notification Messages
    property OnScaleCalculated:   TNotifyEvent        read FScaleCalculated     write FScaleCalculated;
    property OnWidthCalculated:   TNotifyEvent        read FWidthCalculated     write FWidthCalculated;
    property OnHeightCalculated:  TNotifyEvent        read FHeightCalculated    write FHeightCalculated;
    property OnResizingStarted:   TNotifyEvent        read FResizingStarted     write FResizingStarted;
    property OnResizingEnded:     TNotifyEvent        read FResizingEnded       write FResizingEnded;
    property OnDestinationSet:    TNotifyEvent        read FDestinationSet      write FDestinationSet;
    property OnLoadingFile:       TNotifyEvent        read FLoadingFile         write FLoadingFile;
    property OnSavingFile:        TNotifyEvent        read FSavingFile          write FSavingFile;
  end; // TImageResizer

implementation

uses
  SysUtils, Graphics, JPEG, FileCtrl, Math,
  ResizerConstsUnit, ImageFilesUnit, ImageAltercationUnit;

{ TImageResizer }

//////////////////////////////
// Constructor / Destructor //
//////////////////////////////
constructor TImageResizer.Create;
begin
  inherited;

  ///////////////////////////
  // Initialize everything //
  ///////////////////////////
  FImageProcessingMethods := TImageProcessingMethods.Create; // Image processing methods
  FFileInformation        := TFileStructureInformation.Create; // File information object
  FImageInformation       := TImageResizingInformation.Create; // Image resizing information
  FContinueProcess        := True;
  FBadFiles               := TStringList.Create;
  FDirListCurrent         := False;
  FFilesListCurrent       := False;
end;

destructor TImageResizer.Destroy;
begin
  //////////////
  // Clean up //
  //////////////
  FreeAndNil(FImageProcessingMethods);
  FreeAndNil(FFileInformation);
  FreeAndNil(FImageInformation);
  FreeAndNil(FBadFiles);

  inherited;
end;

//////////////////
// Calculations //
//////////////////
function TImageResizer.CalculateScale(const iWidth, iHeight: integer): real;
//////////////////////////////////////////////////////////////////////////////
// Preconditions :                                                          //
//  iWidth, iHeight must be valid width and height, respectively            //
//  FImageInformation.Width, FImageInformation.Height must be set beforehand (0 represents do not constrict)  //
//                                                                          //
// Output :                                                                 //
//  Scale that can be used to calculate width and height to be less than or //
//  equal to the passed in width and height                                 //
//////////////////////////////////////////////////////////////////////////////
var
  fTemp: real;
begin
  if (FImageInformation.Width <> 0) and (FImageInformation.Height <> 0) then
    fTemp := Max(Abs(iWidth / FImageInformation.Width), Abs(iHeight / FImageInformation.Height))
  else
  if (FImageInformation.Width <> 0) then
    fTemp := Abs(iWidth / FImageInformation.Width)
  else
  if (FImageInformation.Height <> 0) then
    fTemp := Abs(iHeight / FImageInformation.Height)
  else
    fTemp := 1.0;

  Result := fTemp;
end; // CalculateScale

function TImageResizer.CalculateWidth(const iWidth: integer): integer;
///////////////////////////////////////////
// Preconditions :                       //
//  FImageInformation.Scale must be valid                 //
//                                       //
// Output :                              //
//  Height value, calculated with FImageInformation.Scale //
///////////////////////////////////////////
begin
  Result := Round(iWidth / FImageInformation.Scale);
end; // CalculateWidth

function TImageResizer.CalculateHeight(const iHeight: integer): integer;
///////////////////////////////////////////
// Preconditions :                       //
//  FImageInformation.Scale must be valid                 //
//                                       //
// Output :                              //
//  Height value, calculated with FImageInformation.Scale //
///////////////////////////////////////////
begin
  Result := Round(iHeight / FImageInformation.Scale);
end; // CalculateHeight

////////////////////
// Business logic //
////////////////////
procedure TImageResizer.SetCurrentWidth(const iCurrentWidth: integer);
////////////////////////////////////////////
// Preconditions :                        //
//  iCurrentWidth must be a valid integer //
//  Should be called when file is loaded  //
//                                        //
// Output :                               //
//  FImageInformation.CurrentWidth is assigned             //
////////////////////////////////////////////
begin
  if iCurrentWidth > 0 then
    FImageInformation.CurrentWidth := iCurrentWidth
  else
    FImageInformation.CurrentWidth := 1;
end;

procedure TImageResizer.SetCurrentHeight(const iCurrentHeight: integer);
/////////////////////////////////////////////
// Preconditions :                         //
//  iCurrentHeight must be a valid integer //
//  Should be called when file is loaded   //
//                                         //
// Output :                                //
//  FImageInformation.CurrentHeight is assigned             //
/////////////////////////////////////////////
begin
  if iCurrentHeight > 0 then
    FImageInformation.CurrentHeight := iCurrentHeight
  else
    FImageInformation.CurrentHeight := 1;
end;

procedure TImageResizer.SetConvertedWidth(const iConvertedWidth: integer);
//////////////////////////////////////////////
// Preconditions :                          //
//  iConvertedWidth must be a valid integer //
//                                          //
// Output :                                 //
//  FImageInformation.ConvertedWidth is assigned             //
//  FWidthCalculated event is called        //
//////////////////////////////////////////////
begin
  FImageInformation.ConvertedWidth := iConvertedWidth;
  if Assigned(FWidthCalculated) then
    FWidthCalculated(Self);
end;

procedure TImageResizer.SetConvertedHeight(const iConvertedHeight: integer);
///////////////////////////////////////////////
// Preconditions :                           //
//  iConvertedHeight must be a valid integer //
//                                           //
// Output :                                  //
//  FImageInformation.ConvertedHeight is assigned             //
//  FHeightCalculated event is called        //
///////////////////////////////////////////////
begin
  FImageInformation.ConvertedHeight := iConvertedHeight;
  if Assigned(FHeightCalculated) then
    FHeightCalculated(Self);
end;

procedure TImageResizer.LogBadFile(const sFileName: String);
///////////////////////////////////////////////////////////////////////////////
// Preconditions :                                                           //
//  sFileName is a valid file name that represents an erroneous file         //
//                                                                           //
// Output :                                                                  //
//  sFileName is added to a list of names of bad files found during resizing //
///////////////////////////////////////////////////////////////////////////////
begin
  FBadFiles.Add(sFileName);
end;

procedure TImageResizer.ClearFilesList;
////////////////////////////////////////
// Preconditions :                    //
//                                    //
// Output :                           //
//  FFileInformation.Files is cleared //
//  FBadFiles is cleared              //
////////////////////////////////////////
begin
  FFileInformation.Files.Clear;
  FBadFiles.Clear;
end;

procedure TImageResizer.PrepareDirsList;
////////////////////////////////////////////////////////////////////
// Preconditions :                                                //
//  FImageProcessingMethods.SourceMethod needs to be set          //
//  to the proper setting                                         //
//                                                                //
// Output :                                                       //
//  FFileInformation.Dirs is populated with a list of directories //
//  to search for files in                                        //
//  FDirListCurrent is reset to True                              //
//                                                                //
// Note :                                                         //
//  Called when FImageProcessingMethods.SourceMethod relates to   //
//  directory-based, not file-based                               //
////////////////////////////////////////////////////////////////////
begin
  FFileInformation.PrepareDirsList(FImageProcessingMethods.SourceMethod);
  FDirListCurrent := True;
end;

procedure TImageResizer.PrepareFilesList;
//////////////////////////////////////////////////////////////////////////
// Preconditions :                                                      //
//  FFileInformation.PrepareFilesList preconditions must be met         //
//                                                                      //
// Output :                                                             //
//  File lists are cleared                                              //
//  FFileInformation.Files is populated with a list of files to process //
//  FFilesListCurrent flag is reset to True                             //
//////////////////////////////////////////////////////////////////////////
begin
  ClearFilesList;
  FFileInformation.PrepareFilesList;

  FFilesListCurrent := True;
end;

/////////////
// Setters //
/////////////
procedure TImageResizer.SetFileName(const sFileName: String);
/////////////////////////////////////////////
// Preconditions :                         //
//  sFileName is a valid file name + path  //
//  Should be called when file is loaded   //
//                                         //
// Output :                                //
//  FImageInformation.FileName is assigned //
//  FLoadingFile is triggered              //
/////////////////////////////////////////////
begin
  FImageInformation.FileName := sFileName;
  if Assigned(FLoadingFile) then
    FLoadingFile(Self);
end;   

procedure TImageResizer.SetScale(fScaleValue: real);
////////////////////////////////////
// Preconditions :                //
//  FImageInformation.Scale is a valid scale value //
//                                //
// Output :                       //
//  FImageInformation.Scale is assigned            //
//  FScaleSet is called           //
////////////////////////////////////
begin
  FImageInformation.Scale := fScaleValue;
  if Assigned(FScaleCalculated) then
    FScaleCalculated(Self);
end;
                    
//////////////////
// Main methods //
//////////////////    
procedure TImageResizer.CalculateImageDimensions;
/////////////////////////////////////////////////////
// Preconditions :                                 //
//  FScalingMethod must be properly set beforehand //
//  FImageInformation.CurrentWidth and FImageInformation.CurrentHeight must be set   //
//                                                 //
// Output :                                        //
//  Scale may be altered                           //
//  FImageInformation.Width, FImageInformation.Height are both calculated              //
/////////////////////////////////////////////////////
begin
  if (FImageProcessingMethods.ScalingMethod = zsmCalculate) then
    Scale := CalculateScale(FImageInformation.CurrentWidth, FImageInformation.CurrentHeight);

  if FImageInformation.Scale <= 0 then
    Scale := 1.0;

  SetConvertedWidth(CalculateWidth(FImageInformation.CurrentWidth));
  SetConvertedHeight(CalculateHeight(FImageInformation.CurrentHeight));
end;

procedure TImageResizer.ResizeAll;
  procedure PrepareOutputDirectory(const sFileName: String);
  ////////////////////////////////////////////////////////////////
  // Preconditions :                                            //
  //  sFileName needs to be a valid file name                   //
  //  FFileInformation.OutputPath needs to be a valid directory //
  //                                                            //
  // Output :                                                   //
  //  Future file directories are created                       //
  ////////////////////////////////////////////////////////////////
  begin
    if not DirectoryExists(ExtractFilePath(sFileName)) then
      ForceDirectories(ExtractFilePath(sFileName));
  end; // procedure PrepareOutputDirectory

  function EverythingIsReady: Boolean;
  var
    bSourcesSet,
    bScaleSet,
    bDestinationSet: Boolean;
  begin
    bSourcesSet     := FFileInformation.Files.Count > 0;

    case FImageProcessingMethods.ScalingMethod of
      zsmFactor:
        bScaleSet   := FImageInformation.Scale > 0;
      zsmCalculate:
        bScaleSet   := (FImageInformation.Width > 0) and (FImageInformation.Height > 0);
    else
      bScaleSet     := False;
    end; // case FScalingMethod

    bDestinationSet := Trim(FFileInformation.OutputPath) <> '';

    Result := bSourcesSet and bScaleSet and bDestinationSet;
  end; // function EverythingIsReady

  function DestinationIsGood(const sFileName: String): Boolean;
  begin
    Result := (FImageProcessingMethods.FileHandling = zfhOverwrite)
      or ((FImageProcessingMethods.FileHandling = zfhSkip) and not FileExists(sFileName));
  end; // function DestinationIsGood

  procedure DoCompressionSettings(const imgImage: TGraphic;
    var imgConverted: TGraphic; iftFileType: TImageFileType);
  /////////////////////////////////////////////////////////////////////////////
  // Preconditions :                                                         //
  //  imgImage and imgConverted are valid TGraphics                          //
  //  iftFileType is a valid TImageFileType                                  //
  //                                                                         //
  // Output :                                                                //
  //  Compression values for the image are set, based on filetype and method //
  /////////////////////////////////////////////////////////////////////////////
  begin
    case iftFileType of
      ziftJPG:
        begin
          if FImageInformation.CompressMethod then
            /////////////////////////////////////////////
            // Use the newly defined compression value //
            /////////////////////////////////////////////
            TJPEGImage(imgConverted).CompressionQuality := FImageInformation.JPEGCompression
          else
            ////////////////////////////////////
            // Use the original image's value //
            ////////////////////////////////////
            TJPEGImage(imgConverted).CompressionQuality := TJPEGImage(imgImage).CompressionQuality;
        end; // ziftJPG
      else
        // Do nothing
    end; // case iftFileType
  end;
var
  bBadFile: Boolean;
  i: integer;
  imgImage,
  imgConverted: TGraphic;
  iftFiletype: TImageFileType;
begin      
  if Assigned(FResizingStarted) then
    FResizingStarted(Self);

  // Validate we have data we need

  // Create directory tree for destination
  case FImageProcessingMethods.SourceMethod of
    zsmRecursiveDirectory:
      begin
        if not FDirListCurrent then
          PrepareDirsList;
        if not FFilesListCurrent then
          PrepareFilesList;
      end; // zsmRecursiveDirectory
    zsmDirectory:
      if not FFilesListCurrent then
        PrepareFilesList;
    // zsmFiles:
  end; // case FSourceMethod

  if EverythingIsReady then
  begin               
    //////////////////
    // Set filetype //
    //////////////////
    if FImageProcessingMethods.FiletypeMethod = zftmPreserve then
      iftFiletype := FileNameToImageFileType(FImageInformation.FileName)
    else
      iftFiletype := FFileInformation.ImageFileType;

    for i := 0 to FFileInformation.Files.Count - 1 do
    begin
      if FContinueProcess and
        DestinationIsGood(
          CreateDestinationFilename(
            FFileInformation.Root,
            FFileInformation.OutputPath,
            FFileInformation.Files.Strings[i],
            ImageFileTypeToExtension(iftFiletype),
            FFileInformation.Prefix,
            FFileInformation.Suffix,
            FImageProcessingMethods.SourceMethod)) then
      begin
        ///////////////
        // Load file //
        ///////////////
        bBadFile := False;

        ///////////////////////////
        // Create as proper type //
        ///////////////////////////
        case FileNameToImageFileType(FFileInformation.Files.Strings[i]) of
          ziftJPG:
            imgImage := TJPEGImage.Create;
        else
          imgImage := TBitmap.Create;
        end; // case FileNameToImageFileType(FFileInformation.Files.Strings[i])

        try
          try
            imgImage.LoadFromFile(FFileInformation.Files.Strings[i]);
          except
            on E:EInvalidGraphic do
              begin
                LogBadFile(E.Message + ' - ' + FFileInformation.Files.Strings[i]);
                bBadFile := True;
                if Assigned(FSavingFile) then
                  FSavingFile(Self);
              end; // EInvalidGraphic
            on E:Exception do
              Raise Exception.Create('Could not finish processing files: ' + E.Message);
          end; // try..except

          if not bBadFile then
          begin
            SetFileName(FFileInformation.Files.Strings[i]);
            SetCurrentWidth(imgImage.Width);
            SetCurrentHeight(imgImage.Height);

            //////////////////////////
            // Perform calculations //
            //////////////////////////
            CalculateImageDimensions;

            //////////////////
            // Resize image //
            //////////////////
            case iftFiletype of
              ziftJPG:
              begin
                imgConverted := ResizeImageJPEG(imgImage, FImageInformation.ConvertedHeight, FImageInformation.ConvertedWidth);
                DoCompressionSettings(imgConverted, imgConverted, iftFiletype);
              end; // ziftJPG
                // TODO: compression!
              ///////////////////////////
              // Not implemented yet : //
              ///////////////////////////
              {
              ziftGIF:
              ziftPNG:
              }
            else
              ///////////////////////
              // Bitmap by default //
              ///////////////////////
              // ziftBMP:
              imgConverted := ResizeImageBMP(imgImage, FImageInformation.ConvertedHeight, FImageInformation.ConvertedWidth);
            end; // case iftFiletype

            try              
              ///////////////
              // Save file //
              ///////////////
              SetDestination(CreateDestinationFilename(FFileInformation.Root,
                FFileInformation.OutputPath,
                FImageInformation.FileName,
                ImageFileTypeToExtension(iftFiletype),
                FFileInformation.Prefix,
                FFileInformation.Suffix,
                FImageProcessingMethods.SourceMethod));

              PrepareOutputDirectory(FFileInformation.Destination);
              if Assigned(FSavingFile) then
                FSavingFile(Self);

              try
              imgConverted.SaveToFile(FFileInformation.Destination);
              except on E:EOutOfResources do
                Raise Exception.Create('Error while saving file: ' + E.Message);
              end;

            finally
              FreeAndNil(imgConverted);
            end; // imgConverted try..finally
          end; // if not bBadFile
        finally
          FreeAndNil(imgImage);
        end; // imgImage try..finally
      end // if FContinueProcess and DestinationIsGood
      else
      begin
        if Assigned(FSavingFile) then
          FSavingFile(Self);
      end; // if not (FContinueProcess and DestinationIsGood)
    end; // for i := 0 to FFileInformation.Files.Count - 1
  end; // if FFileInformation.Files.Count

  if Assigned(FResizingEnded) then
    FResizingEnded(Self);

  if FBadFiles.Count > 0 then
    Raise EInvalidGraphic.Create('Errors were detected with some files.');
end;
             
/////////////
// Getters //
/////////////
// Image Processing Methods
function TImageResizer.GetSourceMethod: TSourceMethod;
begin
  Result := FImageProcessingMethods.SourceMethod;
end;

function TImageResizer.GetFiletypeMethod: TFiletypeMethod;
begin
  Result := FImageProcessingMethods.FiletypeMethod;
end;

function TImageResizer.GetScalingMethod: TScalingMethod;
begin
  Result := FImageProcessingMethods.ScalingMethod;
end;

function TImageResizer.GetFiletypeConversion: TFiletypeConversion;
begin
  Result := FImageProcessingMethods.FiletypeConversion;
end;

function TImageResizer.GetFileHandling: TFileHandling;
begin
  Result := FImageProcessingMethods.FileHandling;
end;

// File Information
function TImageResizer.GetRoot: String;
begin
  Result := FFileInformation.Root;
end;

function TImageResizer.GetDirs: TStringList;
begin
  Result := FFileInformation.Dirs;
end;

function TImageResizer.GetFiles: TStringList;
begin
  Result := FFileInformation.Files;
end;

function TImageResizer.GetOutputPath: String;
begin
  Result := FFileInformation.OutputPath;
end;

function TImageResizer.GetPrefix: String;
begin
  Result := FFileInformation.Prefix;
end;

function TImageResizer.GetSuffix: String;
begin
  Result := FFileInformation.Suffix;
end;

function TImageResizer.GetDestination: String;
begin
  Result := FFileInformation.Destination;
end;

function TImageResizer.GetImageFileType: TImageFileType;
begin
  Result := FFileInformation.ImageFileType;
end;

function TImageResizer.GetImageTypeSet: TImageTypeSet;
begin
  Result := FFileInformation.ImageTypeSet;
end;

// Image Resizing Information
function TImageResizer.GetFileName: String;
begin
  Result := FImageInformation.FileName;
end;

function TImageResizer.GetWidth: integer;
begin
  Result := FImageInformation.Width;
end;

function TImageResizer.GetHeight: integer;
begin
  Result := FImageInformation.Height;
end;
         
function TImageResizer.GetCurrentWidth: integer;
begin
  Result := FImageInformation.CurrentWidth;
end;

function TImageResizer.GetCurrentHeight: integer;
begin
  Result := FImageInformation.CurrentHeight;
end;

function TImageResizer.GetConvertedWidth: integer;
begin
  Result := FImageInformation.ConvertedWidth;
end;

function TImageResizer.GetConvertedHeight: integer;
begin
  Result := FImageInformation.ConvertedHeight;
end; 

function TImageResizer.GetScale: real;
begin
  Result := FImageInformation.Scale;
end;

function TImageResizer.GetCompressMethod: Boolean;
begin
  Result := FImageInformation.CompressMethod;
end;


function TImageResizer.GetJPEGCompression: integer;
begin
  Result := FImageInformation.JPEGCompression;
end;

/////////////
// Setters //
/////////////
// Image Processing Methods
procedure TImageResizer.SetSourceMethod(const Value: TSourceMethod);
begin
  FImageProcessingMethods.SourceMethod := Value;
end;

procedure TImageResizer.SetFiletypeMethod(const Value: TFiletypeMethod);
begin
  FImageProcessingMethods.FiletypeMethod := Value;
end;

procedure TImageResizer.SetScalingMethod(const Value: TScalingMethod);
begin
  FImageProcessingMethods.ScalingMethod := Value;
end;

procedure TImageResizer.SetFiletypeConversion(
  const Value: TFiletypeConversion);
begin
  FImageProcessingMethods.FiletypeConversion := Value;
end;

procedure TImageResizer.SetFileHandling(const Value: TFileHandling);
begin
  FImageProcessingMethods.FileHandling := Value;
end;

// File Information
procedure TImageResizer.SetRoot(const Value: String);
/////////////////////////////////////////////////////////////
// Preconditions :                                         //
//  Value is a valid directory                             //
//                                                         //
// Output :                                                //
//  FFileInformation.Root is assigned                      //
//  FDirListCurrent and FFilesListCurrent are set to False //
/////////////////////////////////////////////////////////////
begin
  FFileInformation.Root := Value;
  FDirListCurrent       := False;
  FFilesListCurrent     := False;
end;

procedure TImageResizer.SetDirs(const Value: TStringList);
begin
  FFileInformation.Dirs := Value;
end;

procedure TImageResizer.SetFiles(const Value: TStringList);
//////////////////////////////////////////////////
// Preconditions :                              //
//  Value has valid files as strings            //
//                                              //
// Output :                                     //
//  File lists are cleared                      //
//  FFileInformation.Files is assigned to Value //
//////////////////////////////////////////////////
begin
  ClearFilesList;
  FFileInformation.Files := Value;
end;

procedure TImageResizer.SetOutputPath(const Value: String);
begin
  FFileInformation.OutputPath := Value;
end;

procedure TImageResizer.SetPrefix(const Value: String);
begin
  FFileInformation.Prefix := Value;
end;

procedure TImageResizer.SetSuffix(const Value: String);
begin
  FFileInformation.Suffix := Value;
end;

procedure TImageResizer.SetDestination(const Value: String);
//////////////////////////////////////////////
// Preconditions :                          //
//  sDestination has a valid, existing path //
//                                          //
// Output :                                 //
//  FFileInformation.Destination is assigned                //
//  FDestinationSet is called               //
//////////////////////////////////////////////
begin
  FFileInformation.Destination := Value;

  if Assigned(FDestinationSet) then
    FDestinationSet(Self);
end;

procedure TImageResizer.SetImageFileType(const Value: TImageFileType);
begin
  FFileInformation.ImageFileType := Value;
end;

procedure TImageResizer.SetImageTypeSet(const Value: TImageTypeSet);
begin
  FFileInformation.ImageTypeSet := Value;
end;

// Image Resizing Information
procedure TImageResizer.SetCompressMethod(const Value: Boolean);
begin
  FImageInformation.CompressMethod := Value;
end;

procedure TImageResizer.SetHeight(const Value: integer);
begin
  FImageInformation.Height := Value;
end;

procedure TImageResizer.SetJPEGCompression(const Value: integer);
begin
  FImageInformation.JPEGCompression := Value;
end;

procedure TImageResizer.SetWidth(const Value: integer);
begin
  FImageInformation.Width := Value;
end;

end.
