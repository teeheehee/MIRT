object frmCompressionSettings: TfrmCompressionSettings
  Left = 305
  Top = 241
  BorderStyle = bsToolWindow
  Caption = 'Compression Settings'
  ClientHeight = 259
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 270
    Top = 232
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 184
    Top = 232
    Width = 75
    Height = 25
    Action = actCancel
    TabOrder = 1
  end
  object pcSettings: TPageControl
    Left = 0
    Top = 8
    Width = 345
    Height = 217
    ActivePage = tsJPEG
    TabOrder = 2
    object tsJPEG: TTabSheet
      Caption = 'JPEG'
      object lblJPEGCompressionLevel: TLabel
        Left = 8
        Top = 16
        Width = 127
        Height = 13
        Caption = 'Image Compression Level :'
      end
      object lblJPEGScale: TLabel
        Left = 50
        Top = 72
        Width = 221
        Height = 13
        Caption = '<---      More compression | Better quality      --->'
      end
      object lblJPEGLevel: TLabel
        Left = 290
        Top = 16
        Width = 23
        Height = 13
        Alignment = taRightJustify
        Caption = '80 %'
      end
      object tbJPEGCompressionLevel: TTrackBar
        Left = 8
        Top = 40
        Width = 305
        Height = 33
        Max = 100
        Frequency = 5
        Position = 80
        TabOrder = 0
        OnChange = tbCompressionLevelChange
      end
    end
  end
  object alActions: TActionList
    Left = 264
    object actOK: TAction
      Category = 'Buttons'
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Category = 'Buttons'
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
end
