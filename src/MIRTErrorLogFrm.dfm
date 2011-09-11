object frmErrorLog: TfrmErrorLog
  Left = 305
  Top = 241
  Width = 537
  Height = 455
  BorderStyle = bsSizeToolWin
  Caption = 'Error Log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    529
    428)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDescription: TLabel
    Left = 4
    Top = 8
    Width = 349
    Height = 13
    Caption = 'Errors were detected when attempting to process these files :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbErrors: TListBox
    Left = 0
    Top = 24
    Width = 529
    Height = 369
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    PopupMenu = pmMenu
    TabOrder = 0
  end
  object btnClose: TButton
    Left = 0
    Top = 400
    Width = 529
    Height = 25
    Action = actClose
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object alActions: TActionList
    Left = 408
    Top = 56
    object actClose: TAction
      Category = 'Buttons'
      Caption = 'Close'
      OnExecute = actCloseExecute
    end
    object actSaveToFile: TAction
      Category = 'Menu'
      Caption = 'Save to file...'
      OnExecute = actSaveToFileExecute
    end
  end
  object pmMenu: TPopupMenu
    Left = 376
    Top = 56
    object miSaveToFile: TMenuItem
      Action = actSaveToFile
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.txt'
    FileName = 'MIRTErrorLog.txt'
    Filter = 'Text files, MIRT Log files|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofEnableSizing]
    Title = 'Save MIRT Error Log'
    Left = 344
    Top = 56
  end
end
