program MIRT;

uses
  Forms,
  ResizerConstsUnit in 'ResizerConstsUnit.pas',
  ImageAltercationUnit in 'ImageAltercationUnit.pas',
  ImageFilesUnit in 'ImageFilesUnit.pas',
  ImageResizerUnit in 'ImageResizerUnit.pas',
  ResizerCommonTypesUnit in 'ResizerCommonTypesUnit.pas',
  FindFilesUnit in 'FindFilesUnit.pas',
  ImageCommonTypesUnit in 'ImageCommonTypesUnit.pas',
  MIRTMainFrm in 'MIRTMainFrm.pas' {frmMIRTMain},
  MIRTErrorLogFrm in 'MIRTErrorLogFrm.pas' {frmErrorLog},
  MIRTCompressionSettingsFrm in 'MIRTCompressionSettingsFrm.pas' {frmCompressionSettings},
  ImageProcessingMethodsUnit in 'ImageProcessingMethodsUnit.pas',
  FileStructureInformationUnit in 'FileStructureInformationUnit.pas',
  ImageResizingInformationUnit in 'ImageResizingInformationUnit.pas',
  CommandLineOperationsUnit in 'CommandLineOperationsUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  /////////////////////////////////////////////////////////////////////
  // Depending on command line parameters, we might not need the GUI //
  /////////////////////////////////////////////////////////////////////
  Application.CreateForm(TfrmMIRTMain, frmMIRTMain);
  Application.Run;
end.
