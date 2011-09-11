unit FileStructureInformationUnit;
/////////////////////////////////////////////////////////////////////////
//                                                                     //
// Description:                                                        //
//  Container for File Information processing                          //
//  Provides a way to build image file list from a root directory      //
//  Also provides output file information                              //
//                                                                     //
// Defaults:                                                           //
//  FRoot           - (blank)                                          //
//  FDirs           - (blank)                                          //
//  FFiles          - (blank)                                          //
//  FOutputPath     - (blank)                                          //
//  FPrefix         - (blank)                                          //
//  FSuffix         - (blank)                                          //
//  FDestination    - (blank)                                          //
//  FImageFileType  - ziftJPG (convert to JPEG if method calls for it) //
//  FImageTypeSet   - [ziftJPG] (JPEG only in the set)                 //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

interface

uses
  Classes, ResizerCommonTypesUnit, ImageCommonTypesUnit;

type
  TFileStructureInformation = class
  private
    FRoot: String;              // root directory
    FDirs: TStringList;         // directory list
    FFiles: TStringList;        // file list
    FOutputPath: String;        // destination path
    FPrefix: String;            // destination filename prefix
    FSuffix: String;            // destination filename suffix
    FDestination: String;       // destination filename + path of current file
    FImageFileType: TImageFileType; // convert to this filetype
    FImageTypeSet: TImageTypeSet;
    /////////////
    // Setters //
    /////////////
    procedure SetFiles(const Value: TStringList);
    procedure SetDirs(const Value: TStringList);
  public
    ////////////////////////////
    // Constructor/Destructor //
    ////////////////////////////
    constructor Create;
    destructor Destroy; reintroduce; overload;
    ////////////////////
    // Business Logic //
    ////////////////////
    procedure PrepareDirsList(const SourceMethod: TSourceMethod);
    procedure PrepareFilesList;
  published
    property Root:          String          read FRoot          write FRoot;
    property Dirs:          TStringList     read FDirs          write SetDirs;
    property Files:         TStringList     read FFiles         write SetFiles;
    property OutputPath:    String          read FOutputPath    write FOutputPath;
    property Prefix:        String          read FPrefix        write FPrefix;
    property Suffix:        String          read FSuffix        write FSuffix;
    property Destination:   String          read FDestination   write FDestination;
    property ImageFileType: TImageFileType  read FImageFileType write FImageFileType;
    property ImageTypeSet:  TImageTypeSet   read FImageTypeSet  write FImageTypeSet;
  end; // TFileStructureInformation

implementation

uses
  SysUtils, FindFilesUnit, ImageFilesUnit;

{ TFileStructureInformation }

////////////////////////////
// Constructor/Destructor //
////////////////////////////
constructor TFileStructureInformation.Create;
begin
  inherited;

  //////////////////////////
  // Intialize everything //
  //////////////////////////
  FRoot           := '';
  FDirs           := TStringList.Create;
  FFiles          := TStringList.Create;
  FOutputPath     := '';
  FPrefix         := '';
  FSuffix         := '';
  FDestination    := '';
  FImageFileType  := ziftJPG;
  FImageTypeSet   := [ziftJPG];
end;

destructor TFileStructureInformation.Destroy;
begin
  //////////////
  // Clean up //
  //////////////
  FreeAndNil(FDirs);
  FreeAndNil(FFiles);

  inherited;
end;

/////////////
// Setters //
/////////////
procedure TFileStructureInformation.SetDirs(const Value: TStringList);
///////////////////////////////////////////////////////
// Preconditions :                                   //
//  Value has valid directories as strings           //
//                                                   //
// Output :                                          //
//  Value is added to FDirs, and then it is resorted //
///////////////////////////////////////////////////////
var
  i: integer;
begin
  if Assigned(Value) and (Value.Count > 0) then
  begin
    for i := 0 to Value.Count - 1 do
      FDirs.Add(Value.Strings[i]);
    FDirs.Sort;
  end; // if Value.Count > 0
end;

procedure TFileStructureInformation.SetFiles(const Value: TStringList);
////////////////////////////////////////////////////////
// Preconditions :                                    //
//  Value has valid files as strings                  //
//                                                    //
// Output :                                           //
//  Value is added to FFiles, and then it is resorted //
////////////////////////////////////////////////////////
var
  i: integer;
begin
  if Assigned(Value) and (Value.Count > 0) then
  begin
    for i := 0 to Value.Count - 1 do
      FFiles.Add(Value.Strings[i]);
    FFiles.Sort;
  end; // if Value.Count > 0
end;
                    
////////////////////
// Business Logic //
////////////////////
procedure TFileStructureInformation.PrepareDirsList(const SourceMethod: TSourceMethod);
///////////////////////////////////////////////////////////////////////////
// Preconditions :                                                       //
//  FRoot must be a valid directory                                      //
//  SourceMethod needs to be set to the proper setting                   //
//                                                                       //
// Output :                                                              //
//  FDirs is populated with a list of directories to search for files in //
//  generated from FRoot                                                 //
//                                                                       //
// Note :                                                                //
//  Called when SourceMethod relates to directory-based, not file-based  //
///////////////////////////////////////////////////////////////////////////
begin
  FDirs.Clear;

  if (FRoot <> '') then
    if (SourceMethod = zsmDirectory) then
      FDirs.Add(IncludeTrailingBackslash(FRoot))
    else
      FDirs := BuildRecursiveDirList(FRoot); // FSourceMethod = zsmRecursiveDirectory
end;

procedure TFileStructureInformation.PrepareFilesList;
//////////////////////////////////////////////////////////
// Preconditions :                                      //
//  FDirs must be prepared                              //
//  FImageTypeSet must be properly set                  //
//                                                      //
// Output :                                             //
//  FFiles is populated with a list of files to process //
//////////////////////////////////////////////////////////
begin
  FFiles := BuildImagesList(FDirs, FImageTypeSet);
end;

end.
