unit MIRTCompressionSettingsFrm;
//////////////////////////////////////////////////////
//                                                  //
// Description: MIRT JPEG Compression Settings form //
//                                                  //
//////////////////////////////////////////////////////

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList;

type
  TfrmCompressionSettings = class(TForm)
    actCancel: TAction;
    actOK: TAction;
    alActions: TActionList;
    btnCancel: TButton;
    btnOK: TButton;
    lblJPEGCompressionLevel: TLabel;
    lblJPEGLevel: TLabel;
    lblJPEGScale: TLabel;
    pcSettings: TPageControl;
    tbJPEGCompressionLevel: TTrackBar;
    tsJPEG: TTabSheet;
    procedure tbCompressionLevelChange(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
  private
    { Private declarations }
    FJPEGCompression: integer;
    procedure SetJPEGCompression(iJPEGCompression: integer);
  public
    { Public declarations }
  published
    property JPEGCompression: integer   read FJPEGCompression   write SetJPEGCompression;
  end;

var
  frmCompressionSettings: TfrmCompressionSettings;

implementation

{$R *.dfm}

procedure TfrmCompressionSettings.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmCompressionSettings.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmCompressionSettings.SetJPEGCompression(
  iJPEGCompression: integer);
begin
  FJPEGCompression                := iJPEGCompression;
  tbJPEGCompressionLevel.Position := FJPEGCompression;
end;

procedure TfrmCompressionSettings.tbCompressionLevelChange(
  Sender: TObject);
begin
  lblJPEGLevel.Caption  := IntToStr(tbJPEGCompressionLevel.Position) + ' %';
  FJPEGCompression      := tbJPEGCompressionLevel.Position;
end;

end.
