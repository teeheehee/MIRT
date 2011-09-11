object frmMIRTMain: TfrmMIRTMain
  Left = 304
  Top = 215
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MIRT (Multiple Image Resizing Tool)'
  ClientHeight = 498
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000660
    0060006000660060006000600000000000000000000000000060006000006600
    0000000000000000000000000060000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000006660
    0000000000000000000000000060000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000006009AAAAAAAAAAAAAAAAA90000000000000AAA
    AAAAAAAAAAAAAAAA0000000000000AAAAAAAAAAAAAAAAAAA0000000000000EAA
    BAAAABAAAAAEAAAA0000000000600E3EBAAEEBEAAAEEE3EE0000000000000222
    EBEEBEE3EEBEE3EE0000000000000B2EEBEEBE222BE223220000000000000EBE
    EEEEBEE2BEEE222E0000000000660EEBEEBBBEEBEEEEE2EE0000000000600EEE
    EBBBBBEEEEEEEEEE0000000000000EEEBBBBBBBEEEEEBBBE0000000000000EEE
    BBBBBBBEEBBBEEEE0000000000600EEEEBBBBBEEEEEEEEEE00000000000009EE
    EEBBBEEEEEEEEEE900000000006000000000000000000000000066006000FFFF
    FFFFB667B8E7ADF7BDDF8D73F8D7B6FFBFEF8FFFFFFF15545555FFFFFDDF3C7F
    BBF5FEEFBBFF7EF7BBDDFEFBB3FF1EFDAB7DFCFE9BFF7EFFB9FDFFFFFFFF0000
    07FD000007FF000007FD000007FF000007FD000007FF000007FD000007FF0000
    07FC000007FD000007FD000007FF000007FD000007FF000007FD00000515}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlProgress: TPanel
    Left = 0
    Top = 434
    Width = 549
    Height = 64
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    object lblSourceFile: TLabel
      Left = 8
      Top = 8
      Width = 53
      Height = 13
      Caption = 'Source File'
    end
    object lblOutputFile: TLabel
      Left = 8
      Top = 48
      Width = 51
      Height = 13
      Caption = 'Output File'
    end
    object lblProgress: TLabel
      Left = 496
      Top = 8
      Width = 41
      Height = 13
      Alignment = taRightJustify
      Caption = 'Progress'
    end
    object lblDimensions: TLabel
      Left = 483
      Top = 48
      Width = 54
      Height = 13
      Alignment = taRightJustify
      Caption = 'Dimensions'
    end
    object prgProgress: TProgressBar
      Left = 8
      Top = 24
      Width = 529
      Height = 17
      TabOrder = 0
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 549
    Height = 434
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object gbSizing: TGroupBox
      Left = 2
      Top = 288
      Width = 545
      Height = 65
      Caption = 'Sizing'
      TabOrder = 4
      object lblWidth: TLabel
        Left = 8
        Top = 44
        Width = 34
        Height = 13
        Caption = 'Width :'
      end
      object lblHeight: TLabel
        Left = 188
        Top = 44
        Width = 37
        Height = 13
        Caption = 'Height :'
      end
      object lblScale: TLabel
        Left = 376
        Top = 44
        Width = 33
        Height = 13
        Caption = 'Scale :'
      end
      object bvlScale: TBevel
        Left = 364
        Top = 16
        Width = 2
        Height = 41
      end
      object spnWidth: TSpinEdit
        Left = 48
        Top = 35
        Width = 121
        Height = 22
        MaxValue = 2147483647
        MinValue = 1
        TabOrder = 2
        Value = 800
      end
      object spnHeight: TSpinEdit
        Left = 232
        Top = 35
        Width = 121
        Height = 22
        MaxValue = 2147483647
        MinValue = 1
        TabOrder = 3
        Value = 600
      end
      object rbDimensions: TRadioButton
        Left = 8
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Dimensions'
        TabOrder = 0
      end
      object rbScale: TRadioButton
        Left = 376
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Scale'
        TabOrder = 1
      end
      object spnScale: TSpinEdit
        Left = 416
        Top = 35
        Width = 121
        Height = 22
        MaxValue = 2147483647
        MinValue = 1
        TabOrder = 4
        Value = 1
      end
    end
    object gbFileInfo: TGroupBox
      Left = 2
      Top = 176
      Width = 545
      Height = 105
      Caption = 'File Info'
      TabOrder = 3
      object lblFilename: TLabel
        Left = 368
        Top = 28
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblSuffix: TLabel
        Left = 192
        Top = 28
        Width = 32
        Height = 13
        Caption = 'Suffix :'
      end
      object lblPrefix: TLabel
        Left = 8
        Top = 28
        Width = 32
        Height = 13
        Caption = 'Prefix :'
      end
      object lblDirectory: TLabel
        Left = 8
        Top = 56
        Width = 48
        Height = 13
        Caption = 'Directory :'
      end
      object edSuffix: TEdit
        Left = 232
        Top = 20
        Width = 121
        Height = 21
        TabOrder = 1
        OnChange = edPrefixChange
      end
      object edPrefix: TEdit
        Left = 48
        Top = 20
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 's_'
        OnChange = edPrefixChange
      end
      object edOutputDirectory: TEdit
        Left = 64
        Top = 48
        Width = 457
        Height = 21
        TabOrder = 3
      end
      object btnOutputDirectory: TButton
        Left = 520
        Top = 48
        Width = 17
        Height = 21
        Action = actSelectDestination
        TabOrder = 2
      end
      object rbPreserve: TRadioButton
        Left = 8
        Top = 80
        Width = 113
        Height = 17
        Caption = 'Preserve filetype'
        TabOrder = 4
      end
      object rbConvert: TRadioButton
        Left = 128
        Top = 80
        Width = 113
        Height = 17
        Caption = 'Enforce filetype :'
        TabOrder = 5
      end
      object cboFiletype: TComboBox
        Left = 232
        Top = 76
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        Items.Strings = (
          'JPEG'
          'Bitmap')
      end
      object chkOverwrite: TCheckBox
        Left = 368
        Top = 80
        Width = 73
        Height = 17
        Caption = 'Overwrite'
        TabOrder = 7
      end
    end
    object lbSource: TListBox
      Left = 1
      Top = 32
      Width = 545
      Height = 137
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 7
    end
    object btnAddFiles: TButton
      Left = 1
      Top = 4
      Width = 75
      Height = 25
      Action = actAddFiles
      TabOrder = 0
    end
    object btnAddDirectory: TButton
      Left = 78
      Top = 4
      Width = 75
      Height = 25
      Action = actAddDirectory
      TabOrder = 1
    end
    object chkRecursive: TCheckBox
      Left = 160
      Top = 12
      Width = 121
      Height = 17
      Caption = 'Recursive directories'
      TabOrder = 2
    end
    object btnRemoveFiles: TButton
      Left = 394
      Top = 4
      Width = 75
      Height = 25
      Action = actRemoveFiles
      TabOrder = 8
    end
    object btnRemoveAll: TButton
      Left = 471
      Top = 4
      Width = 75
      Height = 25
      Action = actClear
      TabOrder = 9
    end
    object btnResize: TButton
      Left = 2
      Top = 404
      Width = 75
      Height = 25
      Action = actResize
      TabOrder = 5
    end
    object btnStop: TButton
      Left = 472
      Top = 404
      Width = 75
      Height = 25
      Action = actStop
      TabOrder = 6
    end
    object gbCompression: TGroupBox
      Left = 2
      Top = 360
      Width = 545
      Height = 37
      Caption = 'Compression'
      TabOrder = 10
      object rbImageCompression: TRadioButton
        Left = 8
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Same as image'
        TabOrder = 0
      end
      object rbEnforceCompression: TRadioButton
        Left = 128
        Top = 16
        Width = 145
        Height = 17
        Caption = 'Use compression settings'
        TabOrder = 1
      end
      object btnCompressionSettings: TButton
        Left = 276
        Top = 12
        Width = 17
        Height = 21
        Action = actCompressionSettings
        TabOrder = 2
      end
    end
  end
  object dlgSourceOpen: TOpenDialog
    DefaultExt = '.jpg'
    Filter = 
      'JPEG|*.jpg; *.jpeg|Bitmap|*.bmp|All Supported|*.jpg; *.jpeg; *.b' +
      'mp'
    InitialDir = 'c:\'
    Options = [ofReadOnly, ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 240
    Top = 40
  end
  object dlgOutputSave: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofEnableSizing]
    Left = 272
    Top = 40
  end
  object alActions: TActionList
    Left = 304
    Top = 40
    object actAddFiles: TAction
      Category = 'Buttons'
      Caption = 'Add Files...'
      OnExecute = actAddFilesExecute
    end
    object actAddDirectory: TAction
      Category = 'Buttons'
      Caption = 'One Directory'
      OnExecute = actAddDirectoryExecute
    end
    object actRemoveFiles: TAction
      Category = 'Buttons'
      Caption = 'Remove Files'
      OnExecute = actRemoveFilesExecute
    end
    object actClear: TAction
      Category = 'Buttons'
      Caption = 'Clear'
      OnExecute = actClearExecute
    end
    object actResize: TAction
      Category = 'Buttons'
      Caption = 'Resize'
      OnExecute = actResizeExecute
    end
    object actStop: TAction
      Category = 'Buttons'
      Caption = 'Stop'
      Enabled = False
      OnExecute = actStopExecute
    end
    object actSelectDestination: TAction
      Category = 'Buttons'
      Caption = '...'
      OnExecute = actSelectDestinationExecute
    end
    object actCompressionSettings: TAction
      Category = 'Buttons'
      Caption = '...'
      OnExecute = actCompressionSettingsExecute
    end
  end
end
