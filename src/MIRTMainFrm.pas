unit MIRTMainFrm;
/////////////////////////////////
//                             //
// Description: MIRT main form //
//                             //
/////////////////////////////////

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, ActnList, ImageResizerUnit;

type
  TfrmMIRTMain = class(TForm)
    actAddDirectory: TAction;
    actAddFiles: TAction;
    actClear: TAction;
    actCompressionSettings: TAction;
    actRemoveFiles: TAction;
    actResize: TAction;
    actSelectDestination: TAction;
    actStop: TAction;
    alActions: TActionList;
    btnAddDirectory: TButton;
    btnAddFiles: TButton;
    btnCompressionSettings: TButton;
    btnOutputDirectory: TButton;
    btnRemoveAll: TButton;
    btnRemoveFiles: TButton;
    btnResize: TButton;
    btnStop: TButton;
    bvlScale: TBevel;
    cboFiletype: TComboBox;
    chkOverwrite: TCheckBox;
    chkRecursive: TCheckBox;
    dlgOutputSave: TSaveDialog;
    dlgSourceOpen: TOpenDialog;
    edOutputDirectory: TEdit;
    edPrefix: TEdit;
    edSuffix: TEdit;
    gbCompression: TGroupBox;
    gbFileInfo: TGroupBox;
    gbSizing: TGroupBox;
    lblDimensions: TLabel;
    lblDirectory: TLabel;
    lblFilename: TLabel;
    lblHeight: TLabel;
    lblOutputFile: TLabel;
    lblPrefix: TLabel;
    lblProgress: TLabel;
    lblScale: TLabel;
    lblSourceFile: TLabel;
    lblSuffix: TLabel;
    lblWidth: TLabel;
    lbSource: TListBox;
    pnlMain: TPanel;
    pnlProgress: TPanel;
    prgProgress: TProgressBar;
    rbConvert: TRadioButton;
    rbDimensions: TRadioButton;
    rbEnforceCompression: TRadioButton;
    rbImageCompression: TRadioButton;
    rbPreserve: TRadioButton;
    rbScale: TRadioButton;
    spnHeight: TSpinEdit;
    spnScale: TSpinEdit;
    spnWidth: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actAddFilesExecute(Sender: TObject);
    procedure actAddDirectoryExecute(Sender: TObject);
    procedure actRemoveFilesExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSelectDestinationExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actResizeExecute(Sender: TObject);
    procedure actCompressionSettingsExecute(Sender: TObject);
    procedure edPrefixChange(Sender: TObject);
    procedure AcceptFiles(var Msg: TMessage); message WM_DROPFILES;
  private
    { Private declarations }
    FImageResizer: TImageResizer;
    FJPEGCompression: integer;

    procedure SetupNotifications;

    procedure ResetAll;
    procedure ResetActions;
    procedure ResetSource;
    procedure ResetFileInfo;
    procedure ResetSizing;
    procedure ResetProgress;
    procedure ResetCompression;
    procedure SetLocks(bValue: Boolean);

    procedure AddMultipleFiles;
    procedure AddDirectoryBtn;
    procedure AddDirectory(const sDir: String);
    procedure DisplayFileName;
    procedure DisplayDimensions;
    procedure RemoveFiles;
    procedure RemoveAllFiles;
    procedure PrepareProgress;
    procedure SetDestinationDirectory;
    procedure SetCompressionSettings;
    procedure UpdateProgressLoad;
    procedure UpdateProgressDestination;
    procedure UpdateProgressSave;

    procedure StopProcessing;
    procedure AlertUser;
    procedure ResizeAll;

    /////////////////////////
    // Notification events //
    /////////////////////////
    procedure OnScaleCalculated(Sender: TObject);
    procedure OnWidthCalculated(Sender: TObject);
    procedure OnHeightCalculated(Sender: TObject);
    procedure OnResizingStarted(Sender: TObject);
    procedure OnResizingEnded(Sender: TObject);
    procedure OnDestinationSet(Sender: TObject);
    procedure OnLoadingFile(Sender: TObject);
    procedure OnSavingFile(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMIRTMain: TfrmMIRTMain;

implementation

uses
  ShellAPI, FileCtrl, ResizerCommonTypesUnit, ImageCommonTypesUnit,
  MIRTErrorLogFrm, ImageFilesUnit, MIRTCompressionSettingsFrm;

{$R *.DFM}

{ TfrmResizerMain }      
                         
/////////////////////////
// Form Create/Destroy //
/////////////////////////
procedure TfrmMIRTMain.FormCreate(Sender: TObject);
begin
  FImageResizer := TImageResizer.Create;

  ////////////////////////////////////////////////////////////////////////
  // Check the command line parameters to see if we need the GUI or not //
  ////////////////////////////////////////////////////////////////////////
                                             
  ///////////////////////////////////////////
  // We need the GUI, so let's start fresh //
  ///////////////////////////////////////////
  ResetAll;
  SetupNotifications;

  //////////////////////////////////////////////////////
  // Tell Windows we're accepting drag and drop files //
  //////////////////////////////////////////////////////
  DragAcceptFiles(Handle, True);
                                                                              
  ////////////////////////////////////////////////////////////////////////////
  // If there were preset values in the command line paramters, accept them //
  ////////////////////////////////////////////////////////////////////////////
                                        
  //////////////////////////////////////
  // NO GUI - Just get some work done //
  //////////////////////////////////////
end;

procedure TfrmMIRTMain.FormDestroy(Sender: TObject);
begin
  if FImageResizer.ContinueProcess then
  begin
    // TODO: Come up with something better here
    // StopProcessing;
    lblProgress.Font.Color := clRed;
    lblProgress.Caption := 'Ending process, please wait 5 seconds...';
    Sleep(5000);
  end; // if FImageResizer.ContinueProcess
  FreeAndNil(FImageResizer);
end;
                   
////////////////
// Form setup //
////////////////   
procedure TfrmMIRTMain.SetupNotifications;
begin
  if Assigned(FImageResizer) then
  begin
    FImageResizer.OnScaleCalculated   := OnScaleCalculated;
    FImageResizer.OnWidthCalculated   := OnWidthCalculated;
    FImageResizer.OnHeightCalculated  := OnHeightCalculated;
    FImageResizer.OnResizingStarted   := OnResizingStarted;
    FImageResizer.OnResizingEnded     := OnResizingEnded;
    FImageResizer.OnDestinationSet    := OnDestinationSet;
    FImageResizer.OnLoadingFile       := OnLoadingFile;
    FImageResizer.OnSavingFile        := OnSavingFile;
  end; // if Assigned FImageResizer
end;

procedure TfrmMIRTMain.ResetAll;
begin
  ResetActions;
  ResetSource;
  ResetFileInfo;
  ResetSizing;
  ResetProgress;
  ResetCompression;
end;

procedure TfrmMIRTMain.ResetFileInfo;
begin
  edPrefix.Text           := 's_';
  edSuffix.Text           := '';
  edOutputDirectory.Text  := '';
  rbConvert.Checked       := True;
  chkOverwrite.Checked    := False;
  cboFiletype.ItemIndex   := cboFiletype.Items.IndexOf('JPEG');
  DisplayFileName;
end;

procedure TfrmMIRTMain.ResetProgress;
begin
  lblSourceFile.Caption := '';
  lblOutputFile.Caption := '';
  lblProgress.Caption   := '';
  lblDimensions.Caption := '';
  prgProgress.Min       := 0;
  prgProgress.Max       := 100;
  prgProgress.Position  := 0;
  prgProgress.Step      := 1;
end;

procedure TfrmMIRTMain.ResetCompression;
begin
  rbImageCompression.Checked  := True;
  FJPEGCompression            := 80; // arbitrary value, good quality
end;

procedure TfrmMIRTMain.ResetSizing;
begin
  rbDimensions.Checked  := True;
  spnWidth.Value        := 800;
  spnHeight.Value       := 600;
  spnScale.Value        := 1;
end;

procedure TfrmMIRTMain.ResetSource;
begin
  chkRecursive.Checked := True;
  lbSource.Clear;
end;

procedure TfrmMIRTMain.ResetActions;
begin
  SetLocks(False);
end;

procedure TfrmMIRTMain.SetLocks(bValue: Boolean);
begin
  /////////////////////////////////////////////////
  // Sets enabled field for all objects, actions //
  //  True   - lock most everything              //
  //  False  - unlock most everything            //
  /////////////////////////////////////////////////
  rbPreserve.Enabled            := not bValue;
  rbConvert.Enabled             := not bValue;
  rbDimensions.Enabled          := not bValue;
  rbScale.Enabled               := not bValue;
  edPrefix.Enabled              := not bValue;
  edSuffix.Enabled              := not bValue;
  edOutputDirectory.Enabled     := not bValue;
  cboFiletype.Enabled           := not bValue;
  chkOverwrite.Enabled          := not bValue;
  spnWidth.Enabled              := not bValue;
  spnHeight.Enabled             := not bValue;
  spnScale.Enabled              := not bValue;

  if FImageResizer.SourceMethod = zsmRecursiveDirectory then
  begin
    actAddDirectory.Enabled     := False;
    actAddFiles.Enabled         := False;
    chkRecursive.Enabled        := False;
  end // if FImageResizer.SourceMethod = zsmRecursiveDirectory
  else
  begin
    actAddDirectory.Enabled     := not bValue;
    actAddFiles.Enabled         := not bValue;
    chkRecursive.Enabled        := not bValue;
  end; // if not FImageResizer.SourceMethod = zsmRecursiveDirectory

  rbImageCompression.Enabled    := not bValue;
  rbEnforceCompression.Enabled  := not bValue;

  actClear.Enabled              := not bValue;
  actRemoveFiles.Enabled        := not bValue;
  actResize.Enabled             := not bValue;
  actSelectDestination.Enabled  := not bValue;
  actCompressionSettings.Enabled := not bValue;
  actStop.Enabled               := bValue;
end;

///////////////////////////////
// Action / Event processing //
///////////////////////////////
procedure TfrmMIRTMain.AddDirectoryBtn;
var
  sDir: String;
begin
  sDir := 'C:\';

  if SelectDirectory(sDir, [], 0) then
  begin
    AddDirectory(sDir);
  end; // if SelectDirectory(sDir, [], 0)
end;

procedure TfrmMIRTMain.AddMultipleFiles;
var
  i: integer;
begin
  dlgSourceOpen.Options := dlgSourceOpen.Options - [];
  dlgSourceOpen.Options := dlgSourceOpen.Options + [ofAllowMultiSelect, ofFileMustExist];
  if dlgSourceOpen.Execute then
  begin
    for i := 0 to dlgSourceOpen.Files.Count - 1 do
      lbSource.Items.Add(dlgSourceOpen.Files[i]);
  end; // if dlgSourceOpen.Execute

  actAddDirectory.Enabled := False;
  actRemoveFiles.Enabled := True;
  chkRecursive.Enabled := False;
end;

procedure TfrmMIRTMain.DisplayFileName;
begin
  lblFilename.Caption := edPrefix.Text + 'FileName' + edSuffix.Text;
end;

procedure TfrmMIRTMain.DisplayDimensions;
begin
  lblDimensions.Caption := IntToStr(FImageResizer.ConvertedWidth) + 'x'
    + IntToStr(FImageResizer.ConvertedHeight) + ' - 1/'
    + FloatToStr(FImageResizer.Scale);
end;

procedure TfrmMIRTMain.RemoveAllFiles;
begin
  lbSource.Clear;
  actAddFiles.Enabled     := True;
  actAddDirectory.Enabled := True;
  chkRecursive.Enabled    := True;
end;

procedure TfrmMIRTMain.RemoveFiles;
var
  i: integer;
begin
  if (lbSource.SelCount = lbSource.Items.Count) then
    RemoveAllFiles
  else
  begin
    if lbSource.SelCount > 0 then
    begin
      for i := (lbSource.Items.Count - 1) downto 0 do
        if lbSource.Selected[i] then
          lbSource.Items.Delete(i);
    end; // if lbSource.SelCount > 0
  end; // if not (lbSource.SelCount = lbSource.Items.Count)
end;

procedure TfrmMIRTMain.PrepareProgress;
begin
  prgProgress.Max := FImageResizer.Files.Count;
end;     

procedure TfrmMIRTMain.SetDestinationDirectory;
var
  sDir: String;
begin
  if SelectDirectory(sDir, [sdAllowCreate, sdPerformCreate, sdPrompt], 1000) then
    edOutputDirectory.Text := sDir;
end;

procedure TfrmMIRTMain.SetCompressionSettings;
var
  frmCompressionSettings: TfrmCompressionSettings;
begin
  frmCompressionSettings := TfrmCompressionSettings.Create(Self);
  try
    frmCompressionSettings.JPEGCompression := FJPEGCompression;
    if (frmCompressionSettings.ShowModal = mrOK) then
    begin
      FJPEGCompression := frmCompressionSettings.JPEGCompression;
    end; // if frmCompressionSettings.ShowModal
  finally
    frmCompressionSettings.Free;
  end;
end;

procedure TfrmMIRTMain.UpdateProgressLoad;
begin
  lblSourceFile.Caption := ExtractFileName(FImageResizer.FileName);
end; 

procedure TfrmMIRTMain.UpdateProgressDestination;
begin
  lblOutputFile.Caption := ExtractFileName(FImageResizer.Destination);
end;

procedure TfrmMIRTMain.UpdateProgressSave;
begin
  prgProgress.StepIt;
  lblProgress.Caption := IntToStr(prgProgress.Position) + ' of '
    + IntToStr(FImageResizer.Files.Count);
end;

procedure TfrmMIRTMain.StopProcessing;
begin
  FImageResizer.ContinueProcess := False;
end;

procedure TfrmMIRTMain.AlertUser;
var
  frmErrorLog: TfrmErrorLog;
begin
  frmErrorLog := TfrmErrorLog.Create(Self);
  try                    
    frmErrorLog.SetErrorLogList(FImageResizer.BadFiles);
    frmErrorLog.ShowModal;
  finally
    frmErrorLog.Free;
  end; // try..finally
end;

procedure TfrmMIRTMain.AddDirectory(const sDir: String);
  procedure AdoptFilesList(slList: TStringList);
  var
    i: integer;
  begin
    if slList.Count > 0 then
      for i := 0 to slList.Count - 1 do
        lbSource.Items.Add(slList.Strings[i]);
  end; // AdoptFilesList
begin
  SetLocks(True);

  actClear.Execute;

  FImageResizer.Root := sDir;

  if chkRecursive.Checked then
    FImageResizer.SourceMethod := zsmRecursiveDirectory
  else
    FImageResizer.SourceMethod := zsmDirectory;

  FImageResizer.PrepareDirsList;
  FImageResizer.PrepareFilesList;
  AdoptFilesList(FimageResizer.Files);

  actAddFiles.Enabled     := False;
  actAddDirectory.Enabled := False;
  actRemoveFiles.Enabled  := False;
  chkRecursive.Enabled    := False;

  SetLocks(False);
end;

procedure TfrmMIRTMain.ResizeAll;
  function SourceFilesToStringList: TStringList;
  var
    slList: TStringList;
    i: integer;
  begin
    if lbSource.Items.Count > 0 then
    begin
      slList := TStringList.Create;
      for i := 0 to lbSource.Items.Count - 1 do
        slList.Add(lbSource.Items[i]);

      Result := slList;
    end // if lbSource.Items.Count > 0
    else
      Result := nil;
  end; // function SourceFilesToStringList
begin
  /////////////////////////////////////////////////////////
  // If we have individual files, we need to put them in //
  /////////////////////////////////////////////////////////
  if actAddFiles.Enabled then
  begin
    FImageResizer.SourceMethod  := zsmFiles;
    FImageResizer.Files         := SourceFilesToStringList;
  end; // if actAddFiles.Enabled

  /////////////////////////////////
  // Get the progress bar set up //
  /////////////////////////////////
  ResetProgress;
  PrepareProgress;

  /////////////////////
  // FileType method //
  /////////////////////
  if rbPreserve.Checked then
    FImageResizer.FileTypeMethod  := zftmPreserve
  else
    FImageResizer.FileTypeMethod  := zftmConvert;

  ///////////////////////
  // FileHandle method //
  ///////////////////////
  if chkOverwrite.Checked then
    FImageResizer.FileHandling    := zfhOverwrite
  else
    FImageResizer.FileHandling    := zfhSkip;

  FImageResizer.Prefix := edPrefix.Text;
  FImageResizer.Suffix := edSuffix.Text;
  // TODO: make this better
  if CompareText(cboFiletype.Text, 'JPEG') = 0 then
    FImageResizer.ImageFileType := ziftJPG
  else
    FImageResizer.ImageFileType := ziftBMP;
  FImageResizer.OutputPath := edOutputDirectory.Text;

  ////////////////////
  // Scaling method //
  ////////////////////
  if rbDimensions.Checked then
    FImageResizer.ScalingMethod := zsmCalculate
  else
    FImageResizer.ScalingMethod := zsmFactor;

  FImageResizer.Width := spnWidth.Value;
  FImageResizer.Height := spnHeight.Value;
  FImageResizer.Scale := spnScale.Value;
                            
  //////////////////////////
  // Compression settings //
  //////////////////////////
  FImageResizer.CompressMethod := rbEnforceCompression.Checked;
  FImageResizer.JPEGCompression := FJPEGCompression;

  FImageResizer.ContinueProcess := True;
  try
    FImageResizer.ResizeAll;
  except
    on E:EInvalidGraphic do
    begin
      SetLocks(False);
      AlertUser;
    end; // on E:EInvalidGraphic
    on E:Exception do
    begin
      SetLocks(False);
      AlertUser;
      Raise;
    end; // on E:Exception
  end; // try..except
end;

/////////////////////////////
// Action / Event handling //
/////////////////////////////
procedure TfrmMIRTMain.OnHeightCalculated(Sender: TObject);
begin
  DisplayDimensions;
  Application.ProcessMessages;
end;

procedure TfrmMIRTMain.OnWidthCalculated(Sender: TObject);
begin
  DisplayDimensions;
  Application.ProcessMessages;
end;          

procedure TfrmMIRTMain.OnScaleCalculated(Sender: TObject);
begin
  DisplayDimensions;
  Application.ProcessMessages;
end;

procedure TfrmMIRTMain.OnResizingEnded(Sender: TObject);
begin
  SetLocks(False);
end;

procedure TfrmMIRTMain.OnResizingStarted(Sender: TObject);
begin
  SetLocks(True);
end;

procedure TfrmMIRTMain.OnDestinationSet(Sender: TObject);
begin
  UpdateProgressDestination;
  Application.ProcessMessages;
end;

procedure TfrmMIRTMain.OnLoadingFile(Sender: TObject);
begin
  UpdateProgressLoad;
  Application.ProcessMessages;
end;

procedure TfrmMIRTMain.OnSavingFile(Sender: TObject);
begin
  UpdateProgressSave;
  Application.ProcessMessages;
end;

procedure TfrmMIRTMain.actAddFilesExecute(Sender: TObject);
begin
  AddMultipleFiles;
end;

procedure TfrmMIRTMain.actAddDirectoryExecute(Sender: TObject);
begin
  AddDirectoryBtn;
end;

procedure TfrmMIRTMain.actRemoveFilesExecute(Sender: TObject);
begin
  RemoveFiles;
end;

procedure TfrmMIRTMain.actClearExecute(Sender: TObject);
begin
  RemoveAllFiles;
end;

procedure TfrmMIRTMain.actSelectDestinationExecute(Sender: TObject);
begin
  SetDestinationDirectory;
end;

procedure TfrmMIRTMain.actStopExecute(Sender: TObject);
begin
  StopProcessing;
end;

procedure TfrmMIRTMain.actResizeExecute(Sender: TObject);
begin
  ResizeAll;
end;

procedure TfrmMIRTMain.actCompressionSettingsExecute(Sender: TObject);
begin
  SetCompressionSettings;
end;

procedure TfrmMIRTMain.edPrefixChange(Sender: TObject);
begin
  DisplayFileName;
end;

procedure TfrmMIRTMain.AcceptFiles(var Msg: TMessage);
const
  MAXFILENAMELEN = 1023;
var
  i,
  iCount: integer;
  acFileName: array [0..MAXFILENAMELEN] of char;
begin
  /////////////////////////////////////////////
  // Find out how many files we're accepting //
  /////////////////////////////////////////////
  iCount := DragQueryFile(msg.WParam,
    $FFFFFFFF,
    acFileName,
    MAXFILENAMELEN);

  ///////////////////////////////////////////////////
  // Query Windows one at a time for the file name //
  ///////////////////////////////////////////////////
  for i := 0 to (iCount - 1) do
  begin
    DragQueryFile(msg.WParam,
      i,
      acFileName,
      MAXFILENAMELEN);

    ////////////////////////////////////////////////////////////////
    // Check to see if what was dropped was a directory or a file //
    ////////////////////////////////////////////////////////////////
    if DirectoryExists(String(acFileName)) then
      ///////////////
      // Directory //
      ///////////////
      AddDirectory(String(acFileName))
    else
    if IsGraphicsFile(String(acFileName)) then
      ///////////////////////////////////////////////////
      // TODO: Validate files added are graphics files //
      ///////////////////////////////////////////////////
      ///////////////////////////////
      // Add files to the list box //
      ///////////////////////////////
      lbSource.Items.Add(String(acFileName));
  end;
                
  actAddDirectory.Enabled := False;
  chkRecursive.Enabled := False;
                        
  /////////////////////////////////////////////
  // Let Windows know that operation is done //
  /////////////////////////////////////////////
  DragFinish(Msg.WParam);
end;

end.

