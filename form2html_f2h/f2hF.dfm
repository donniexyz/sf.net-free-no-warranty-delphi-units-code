object f2hFrm: Tf2hFrm
  Left = 255
  Top = 116
  HorzScrollBar.Visible = False
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'TWHForm2HTML attributes editor'
  ClientHeight = 414
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 13
  object hintBar: TPanel
    Left = 0
    Top = 350
    Width = 616
    Height = 27
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    TabOrder = 3
  end
  object panLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 350
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object phCtls: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 23
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 0
      object labCtls: TLabel
        Left = 7
        Top = 5
        Width = 51
        Height = 13
        Caption = '&Controls:'
        FocusControl = treeCtls
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object treeCtls: TTreeView
      Left = 0
      Top = 23
      Width = 185
      Height = 327
      Align = alClient
      Ctl3D = True
      HideSelection = False
      Indent = 16
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 1
      OnChange = treeCtlsChange
    end
  end
  object panRight: TPanel
    Left = 186
    Top = 0
    Width = 430
    Height = 350
    BevelOuter = bvNone
    TabOrder = 1
    object phAttrs: TPanel
      Left = 0
      Top = 0
      Width = 430
      Height = 23
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 0
      object labAttrs: TLabel
        Left = 7
        Top = 5
        Width = 59
        Height = 13
        Caption = '&Attributes:'
        FocusControl = grid
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object grid: TDrawGrid
      Left = 0
      Top = 23
      Width = 430
      Height = 327
      Align = alClient
      ColCount = 2
      DefaultColWidth = 105
      DefaultRowHeight = 17
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing]
      TabOrder = 1
      OnDblClick = gridDblClick
      OnDrawCell = gridDrawCell
      OnEnter = gridEnter
      OnExit = gridExit
      OnGetEditText = gridGetEditText
      OnKeyDown = gridKeyDown
      OnKeyPress = gridKeyPress
      OnMouseMove = gridMouseMove
      OnSelectCell = gridSelectCell
      OnSetEditText = gridSetEditText
    end
  end
  object panBottom: TPanel
    Left = 0
    Top = 377
    Width = 616
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object butOK: TButton
      Left = 9
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 0
      OnClick = butOKClick
    end
    object butCancel: TButton
      Left = 98
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
    end
    object butExport: TButton
      Left = 188
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'E&xport...'
      TabOrder = 2
      OnClick = butExportClick
    end
    object butBrowse: TButton
      Left = 388
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Browse it!'
      TabOrder = 4
      OnClick = butBrowseClick
    end
    object butDefault: TButton
      Left = 477
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Default'
      TabOrder = 5
      OnClick = butDefaultClick
    end
    object chkWH: TCheckBox
      Left = 278
      Top = 12
      Width = 97
      Height = 17
      Caption = 'WebHub chunk'
      TabOrder = 3
    end
  end
end
