unit MIRTErrorLogFrm;
//////////////////////////////////////
//                                  //
// Description: MIRT Error Log form //
//                                  //
//////////////////////////////////////

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls, Menus;

type
  TfrmErrorLog = class(TForm)
    lbErrors: TListBox;
    btnClose: TButton;
    lblDescription: TLabel;
    alActions: TActionList;
    actClose: TAction;
    pmMenu: TPopupMenu;
    miSaveToFile: TMenuItem;
    actSaveToFile: TAction;
    dlgSave: TSaveDialog;
    procedure actCloseExecute(Sender: TObject);
    procedure actSaveToFileExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetErrorLogList(slLog: TStringList);
  end;

implementation

{$R *.DFM}

procedure TfrmErrorLog.actCloseExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmErrorLog.SetErrorLogList(slLog: TStringList);
var
  i: integer;
begin
  lbErrors.Clear;
  if slLog.Count > 0 then
    for i := 0 to slLog.Count - 1 do
      lbErrors.Items.Add(slLog.Strings[i]);
end;

procedure TfrmErrorLog.actSaveToFileExecute(Sender: TObject);
var
  i: integer;
  slLog: TStringList;
begin
  slLog := TStringList.Create;
  try
    if dlgSave.Execute then
    begin
      slLog.Add('MIRT ERROR LOG, PROCESSED ON ' + DateTimeToStr(Now));
      slLog.Add('Skipped ' + IntToStr(lbErrors.Items.Count) + ' files :');
      slLog.Add('');

      if lbErrors.Items.Count > 0 then
        for i := 0 to lbErrors.Items.Count - 1 do
          slLog.Add(lbErrors.Items[i]);

      slLog.SaveToFile(dlgSave.FileName);
    end; // if dlgSave.Execute
  finally
    slLog.Free;
  end; // try..finally
end;

end.
