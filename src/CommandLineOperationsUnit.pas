unit CommandLineOperationsUnit;

interface

uses
  Classes;

type
  TCommandLineOperations = class
  public
    ////////////////////////////
    // Constructor/Destructor //
    ////////////////////////////
    constructor Create(const sCommandLine: String);
    destructor Destroy; reintroduce; overload;

    // Validators
    {
    function ValidateCommandLineParses: Boolean; // top level check, is syntax correct?

    function ValidateImageListFile(const sFilename: String): Boolean;
    function ValidateSourceMethod: Boolean; // method is set, image list validates if necessary, source validates
    function ValidateCompressMethod: Boolean; // method is set, value set if override chosen
    function ValidateSource: Boolean; // directory exists/files exist
    function ValidateFileTypeMethod: Boolean; // method is set, image file type set if convert is choice
    function ValidateFileHandling: Boolean; // method is set
    function ValidatePreSuffix: Boolean; // values exist if flag is set
    function ValidateOutputPath: Boolean; // output path exists/can be created
    function ValidateScalingMethod: Boolean; // scaling method set, and accompanying width, height, and/or scale values

    // continue?
    procedure ParseCommandLineParams; // set up objects ???
    }
  private
    {
    FImageProcessingMethods: TImageProcessingMethods; // Image processing methods
    FFileInformation: TFileStructureInformation;      // File structure information
    FImageInformation: TImageResizingInformation;     // Image resizing information
    }

    FGUI: Boolean;                      // do we need a GUI or not?
    FValidCommandLineOptions: Boolean;  // are the options operable?
    // FBadFiles: TStringList;             // List of bad files found during resizing
  published
  end; // TCommandLineOperations

implementation

{ TCommandLineOperations }

constructor TCommandLineOperations.Create(const sCommandLine: String);
begin
  inherited Create;

  ///////////////////////////
  // Initialize everything //
  ///////////////////////////
  {
  FImageProcessingMethods := TImageProcessingMethods.Create; // Image processing methods
  FFileInformation        := TFileStructureInformation.Create; // File information object
  FImageInformation       := TImageResizingInformation.Create; // Image resizing information
  }
  FGUI                    := True; // assume we need the GUI
  // FBadFiles               := TStringList.Create;
end;

destructor TCommandLineOperations.Destroy;
begin
  //////////////
  // Clean up //
  //////////////
  {
  FreeAndNil(FImageProcessingMethods);
  FreeAndNil(FFileInformation);
  FreeAndNil(FImageInformation);
  FreeAndNil(FBadFiles);
  }
  inherited;
end;

end.
 