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

unit dxPScxTLLnkReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf, DesignEditors,
  Classes, Controls, StdCtrls, cxStyles, cxStyleSheetEditor, cxCustomData, cxTL,
  cxImage, dxPSRes, dxPScxTLLnk, dxPScxCommon, dxPSReg;

type
  TcxTreeListReportLinkStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FTreeList: TcxTreeList;
  protected
    procedure Initialize; virtual;
    property TreeList: TcxTreeList read FTreeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;

    function Control: TWinControl; override;

    class procedure Register;
    class procedure Unregister;
  end;

  TcxCustomTreeListAccess = class(TcxCustomTreeList);

{ TcxTreeListReportLinkStyleSheetEditorPreview }

constructor TcxTreeListReportLinkStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
//  inherited; cannot call ancestor method if it's an abstract method in Delphi4(5) !!!!
  FTreeList := TcxTreeList.Create(AOwner);
  Initialize;
end;

destructor TcxTreeListReportLinkStyleSheetEditorPreview.Destroy;
begin
  FTreeList.Free;
  inherited;
end;

class function TcxTreeListReportLinkStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxTreeListReportLinkStyleSheet;
end;

procedure TcxTreeListReportLinkStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
var
  TreeListStyles: TcxTreeListStyles;
begin
  if AStyleSheet is TcxTreeListReportLinkStyleSheet then
  begin
    TreeListStyles := TcxCustomTreeListAccess(TreeList).Styles;
    with TcxTreeListReportLinkStyleSheet(AStyleSheet).Styles do
    begin
      TreeListStyles.BandHeader := BandHeader;
      TreeListStyles.ColumnFooter := Footer;
      TreeListStyles.ColumnHeader := Header;
      TreeListStyles.Content := Content;
      TreeListStyles.ContentEven := ContentEven;
      TreeListStyles.ContentOdd := ContentOdd;
      TreeListStyles.Footer := FooterRow;
      TreeListStyles.Preview := Preview;
    end;
  end
  else
    TreeList.Styles.ResetStyles;
end;

function TcxTreeListReportLinkStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := TreeList;
end;

class procedure TcxTreeListReportLinkStyleSheetEditorPreview.Register;
begin
  cxStyleSheetEditor.RegisterStyleSheetEditorPreview(Self);
end;

class procedure TcxTreeListReportLinkStyleSheetEditorPreview.Unregister;
begin
  cxStyleSheetEditor.UnregisterStyleSheetEditorPreview(Self);
end;

procedure TcxTreeListReportLinkStyleSheetEditorPreview.Initialize;
const
  cBandCount = 2;
  cColumnCount = 5;
  cNodeCount = 4;

  procedure InitializeBands;
  const
    BandCaptions: array[0..cBandCount - 1] of string = (sdxManufacturerBandCaption, sdxModelBandCaption);
    BandWidths: array[0..cBandCount - 1] of Integer = (300, 200);
  var
    I: Integer;
  begin
    for I := 0 to cBandCount - 1 do
      with TcxCustomTreeListAccess(TreeList).Bands.Add do
      begin
        Caption.Text := BandCaptions[I];
        Width := BandWidths[I];
      end;
  end;

  procedure InitializeColumns;
  const
    ColumnBandIndexes: array[0..cColumnCount - 1] of Integer = (0, 0, 1, 1, 1);
    ColumnCaptions: array[0..cColumnCount - 1] of string =
      (sdxManufacturerNameColumnCaption, sdxManufacturerLogoColumnCaption, sdxManufacturerCountryColumnCaption,
       sdxCarModelColumnCaption, sdxCarIsSUVColumnCaption);
    ColumnProperties: array[0..cColumnCount - 1] of string =
      ('TcxTextEditProperties', 'TcxImageProperties', 'TcxTextEditProperties',
       'TcxTextEditProperties', 'TcxCheckBoxProperties');
    ColumnWidths: array[0..cColumnCount - 1] of Integer = (150, 150, 0, 120, 80);
  var
    I: Integer;
  begin
    with TcxCustomTreeListAccess(TreeList) do
    begin
      for I := 0 to cColumnCount - 1 do
        with CreateColumn do
        begin
          Caption.Text := ColumnCaptions[I];
          Position.BandIndex := ColumnBandIndexes[I];
          Position.ColIndex := Bands[ColumnBandIndexes[I]].ColumnCount - 1;
          PropertiesClassName := ColumnProperties[I];
          Width := ColumnWidths[I];
        end;

      with Columns[0].Summary.FooterSummaryItems.Add do
      begin
        Kind := skCount;
        Format := sdxSummaryFormat;
      end;

      TcxImageProperties(Columns[1].Properties).GraphicTransparency := gtTransparent;

      Preview.Column := Columns[2];
      Preview.Visible := True;
    end;
  end;

  procedure InitializeData;
  const
    ManufacturerNames: array[0..cNodeCount - 1] of string =
      (sdxCarManufacturerName5, sdxCarManufacturerName1, sdxCarManufacturerName2, sdxCarManufacturerName4);
    ManufacturerCountries: array[0..cNodeCount - 1] of string =
      (sdxCarManufacturerCountry5, sdxCarManufacturerCountry1, sdxCarManufacturerCountry2, sdxCarManufacturerCountry4);
    CarLogosIndexes: array[0..cNodeCount - 1] of Integer =
      (4, 0, 1, 3);
    CarModels: array[0..cNodeCount - 1] of string =
      (sdxCarModel5, sdxCarModel1, sdxCarModel2, sdxCarModel4);
    AreCarsSUV: array[0..cNodeCount - 1] of string = ('False', 'True', 'True', 'True');
  var
    I: Integer;
  begin
    for I := 0 to cNodeCount - 1 do
      with TreeList.Add do
      begin
        Values[0] := ManufacturerNames[I];
        Values[1] := dxPScxCommon.dxPSPreviewCarLogosAsString(CarLogosIndexes[I]);
        Values[2] := ManufacturerCountries[I];
        Values[3] := CarModels[I];
        Values[4] := AreCarsSUV[I];
      end;
  end;

  procedure InitializeOptions;
  begin
    with TcxCustomTreeListAccess(TreeList) do
    begin
      OptionsView.Bands := True;
      OptionsView.CellAutoHeight := True;
      OptionsView.ColumnAutoWidth := True;
      OptionsView.Footer := True;
      OptionsView.UseNodeColorForIndent := False;

      OptionsData.Editing := False;
    end;
  end;

begin
  InitializeBands;
  InitializeColumns;
  InitializeData;
  InitializeOptions;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterNoIcon([TcxTreeListReportLink, TcxDBTreeListReportLink,
    TcxVirtualTreeListReportLink, TcxTreeListReportLinkStyleSheet]);
  dxPSRegisterReportLinkUnit('dxPScxTLLnk', TcxVirtualTreeListReportLink);
  dxPSRegisterReportLinkUnit('dxPScxTLLnk', TcxTreeListReportLink);
end;

procedure RegisterStyleSheetAssistants;
begin
  cxStyles.RegisterStyleSheetClass(TcxTreeListReportLinkStyleSheet);
  TcxTreeListReportLinkStyleSheetEditorPreview.Register;
end;

procedure UnregisterStyleSheetAssistants;
begin
  TcxTreeListReportLinkStyleSheetEditorPreview.Unregister;
  cxStyles.UnregisterStyleSheetClass(TcxTreeListReportLinkStyleSheet);
end;

initialization
  RegisterStyleSheetAssistants;

finalization
  UnregisterStyleSheetAssistants;

end.
