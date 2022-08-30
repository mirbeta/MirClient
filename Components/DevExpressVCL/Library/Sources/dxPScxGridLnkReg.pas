{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPScxGridLnkReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf, DesignEditors,
  SysUtils, Classes, Controls, cxCustomData, cxDataStorage, cxStyles, cxGrid,
  cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridCardView,
  cxGridTableView, cxGridBandedTableView, cxStyleSheetEditor, dxPScxGridLnk, dxPScxGridLayoutViewLnk,
  dxPSReg;

type

  { TdxGridReportLinkStyleSheetEditorPreview }

  TdxGridReportLinkStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FGrid: TcxGrid;
    function GetBandedTableView: TcxGridBandedTableView;
    function GetCardView: TcxGridCardView;
  protected
    function AddItem(AView: TcxCustomGridTableView; const ACaption: string;
      AValueTypeClass: TcxValueTypeClass): TcxCustomGridTableItem;
    procedure AddItems(AView: TcxCustomGridTableView);
    procedure CreateBandedTableView;
    procedure CreateCardView;
    procedure Initialize; virtual;

    property BandedTableView: TcxGridBandedTableView read GetBandedTableView;
    property CardView: TcxGridCardView read GetCardView;
    property Grid: TcxGrid read FGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;

    function Control: TWinControl; override;

    class procedure Register;
    class procedure Unregister;
  end;

{ TdxGridReportLinkStyleSheetEditorPreview }

constructor TdxGridReportLinkStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
//  inherited; cannot call ancestor method if it's an abstract method in Delphi4(5) !!!!
  FGrid := TcxGrid.Create(AOwner);
  Initialize;
end;

destructor TdxGridReportLinkStyleSheetEditorPreview.Destroy;
begin
  FGrid.Free;
  inherited;
end;

class function TdxGridReportLinkStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TdxGridReportLinkStyleSheet;
end;

procedure TdxGridReportLinkStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  if AStyleSheet is TdxGridReportLinkStyleSheet then
    with TdxGridReportLinkStyleSheet(AStyleSheet).Styles do
    begin
      BandedTableView.Styles.BandHeader := BandHeader;
      BandedTableView.Styles.Content := Content;
      BandedTableView.Styles.ContentEven := ContentEven;
      BandedTableView.Styles.ContentOdd := ContentOdd;
      BandedTableView.Styles.FilterBox := FilterBar;
      BandedTableView.Styles.Footer := Footer;
      BandedTableView.Styles.Group := Group;
      BandedTableView.Styles.Header := Header;
      BandedTableView.Styles.Indicator := Header;
      BandedTableView.Styles.Preview := Preview;

      CardView.Styles.CaptionRow := CardCaptionRow;
      CardView.Styles.Content := Content;
      CardView.Styles.ContentEven := ContentEven;
      CardView.Styles.ContentOdd := ContentOdd;
      CardView.Styles.RowCaption := CardRowCaption;

      Grid.RootLevelStyles.Tab := Caption;
      Grid.RootLevelStyles.TabsBackground := Caption;
    end
  else
  begin
    BandedTableView.Styles.ResetStyles;
    CardView.Styles.ResetStyles;
    Grid.RootLevelStyles.ResetStyles;
  end;
end;

function TdxGridReportLinkStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := Grid;
end;

class procedure TdxGridReportLinkStyleSheetEditorPreview.Register;
begin
  cxStyleSheetEditor.RegisterStyleSheetEditorPreview(Self);
end;

class procedure TdxGridReportLinkStyleSheetEditorPreview.Unregister;
begin
  cxStyleSheetEditor.UnregisterStyleSheetEditorPreview(Self);
end;

function TdxGridReportLinkStyleSheetEditorPreview.AddItem(AView: TcxCustomGridTableView;
  const ACaption: string;  AValueTypeClass: TcxValueTypeClass): TcxCustomGridTableItem;
begin
  Result := AView.CreateItem;
  with Result do
  begin
    Caption := ACaption;
    DataBinding.ValueTypeClass := AValueTypeClass;
  end;
end;

procedure TdxGridReportLinkStyleSheetEditorPreview.AddItems(AView: TcxCustomGridTableView);
begin
  AddItem(AView, 'Text', TcxStringValueType);
  AddItem(AView, 'Currency', TcxCurrencyValueType);
  AddItem(AView, 'Date', TcxDateTimeValueType);
end;

procedure TdxGridReportLinkStyleSheetEditorPreview.CreateBandedTableView;

  procedure AddBands;
  begin
    BandedTableView.Bands.Add.Caption := 'Band';
  end;

  procedure AddGroups;
  begin
    BandedTableView.Columns[2].GroupIndex := 0;
  end;

  procedure AddPreview;
  begin
    with BandedTableView.Preview do
    begin
      Column := AddItem(BandedTableView, '', TcxStringValueType) as TcxGridColumn;
      Column.Visible := False;
      Visible := True;
    end;
  end;

  procedure AddRecords;
  const
    ARecordCount: Integer = 3;
  var
    I: Integer;
  begin
    with BandedTableView.DataController as TcxGridDataController do
    begin
      BeginUpdate;
      try
        RecordCount := ARecordCount;
        for I := 0 to ARecordCount - 1 do
        begin
          Values[I, 0] := 'ABC';
          Values[I, 1] := (I + 1) * 100;
          Values[I, 2] := Date - I;
          Values[I, 3] := 'Check the preview style';
        end;
      finally
        EndUpdate;
      end;
    end;
  end;

  procedure AddSummaries;
  begin
    with BandedTableView do
    begin
      DataController.Groups.FullExpand;
      with DataController.Summary.FooterSummaryItems.Add do
      begin
        Kind := skSum;
        ItemLink := Columns[1];
      end;
    end;
  end;

  procedure InitializeOptions;
  begin
    with BandedTableView do
    begin
      Filtering.CustomizeDialog := False;

      OptionsBehavior.IncSearch := True;

      OptionsData.Deleting := False;
      OptionsData.Editing := False;
      OptionsData.Appending := False;
      OptionsData.Inserting := False;

      OptionsView.BandHeaders := True;
      OptionsView.ColumnAutoWidth := True;
      OptionsView.Footer := True;
      OptionsView.Indicator := True;
    end;
  end;

  procedure PlaceColumns;
  var
    I: Integer;
  begin
    with BandedTableView do
      for I := 0 to ItemCount - 1 do
        Columns[I].Position.BandIndex := 0;
  end;

begin
  Grid.Levels.Add.GridView := Grid.CreateView(TcxGridBandedTableView);
  TcxGridLevel(BandedTableView.Level).Caption := 'Table';

  AddBands;
  AddItems(BandedTableView);
  AddGroups;
  AddPreview;
  AddRecords;
  AddSummaries;
  InitializeOptions;
  PlaceColumns;
end;

procedure TdxGridReportLinkStyleSheetEditorPreview.CreateCardView;

  procedure AddRecords;
  const
    ARecordCount: Integer = 3;
  var
    I: Integer;
  begin
    with CardView.DataController as TcxGridDataController do
    begin
      BeginUpdate;
      try
        RecordCount := ARecordCount;
        for I := 0 to ARecordCount - 1 do
        begin
          Values[I, 1] := 'ABC';
          Values[I, 2] := (I + 1) * 100;
          Values[I, 3] := Date - I;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;

  procedure AddCaptionRow;
  begin
    with CardView.CreateRow do
    begin
      Caption := 'Caption';
      Index := 0;
      Kind := rkCaption;
    end;
  end;

  procedure InitializeOptions;
  begin
    with CardView do
    begin
      OptionsBehavior.IncSearch := True;

      OptionsData.Deleting := False;
      OptionsData.Editing := False;
      OptionsData.Appending := False;
      OptionsData.Inserting := False;
    end;
  end;

begin
  Grid.Levels.Add.GridView := Grid.CreateView(TcxGridCardView);
  TcxGridLevel(CardView.Level).Caption := 'Cards';

  AddItems(CardView);
  AddCaptionRow;
  AddRecords;
  InitializeOptions;
end;

procedure TdxGridReportLinkStyleSheetEditorPreview.Initialize;
begin
  CreateBandedTableView;
  CreateCardView;

  Grid.RootLevelOptions.DetailTabsPosition := dtpTop;
end;

function TdxGridReportLinkStyleSheetEditorPreview.GetBandedTableView: TcxGridBandedTableView;
begin
  Result := Grid.Levels[0].GridView as TcxGridBandedTableView;
end;

function TdxGridReportLinkStyleSheetEditorPreview.GetCardView: TcxGridCardView;
begin
  Result := Grid.Levels[1].GridView as TcxGridCardView;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  dxPSRegisterReportLinkUnit('dxPScxGridLnk', TdxGridReportLink);
  dxPSRegisterReportLinkUnit('dxPScxGridLayoutViewLnk', TdxGridReportLink);
  RegisterNoIcon([TdxGridReportLink, TdxGridReportLinkStyleSheet]);
end;

procedure RegisterStyleSheetAssistants;
begin
  cxStyles.RegisterStyleSheetClass(TdxGridReportLinkStyleSheet);
  TdxGridReportLinkStyleSheetEditorPreview.Register;
end;

procedure UnregisterStyleSheetAssistants;
begin
  TdxGridReportLinkStyleSheetEditorPreview.Unregister;
  cxStyles.UnregisterStyleSheetClass(TdxGridReportLinkStyleSheet);
end;

initialization
  RegisterStyleSheetAssistants;

finalization
  UnregisterStyleSheetAssistants;

end.
