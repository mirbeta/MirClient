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

unit dxPScxVGridLnkReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf, DesignEditors,
  Windows, Classes, Controls, StdCtrls, cxStyles, cxStyleSheetEditor,
  cxCustomData, cxVGrid, cxEdit, cxTextEdit, cxMemo, cxImage, dxPSRes, dxPSUtl,
  dxPScxCommon, dxPScxVGridLnk, dxPSReg;

type
  TcxVerticalGridReportLinkStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FVerticalGrid: TcxVerticalGrid;
  protected
    procedure Initialize; virtual;
    property VerticalGrid: TcxVerticalGrid read FVerticalGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;

    function Control: TWinControl; override;
    function GetSize: TPoint; override;

    class procedure Register;
    class procedure Unregister;
  end;

{ TcxVerticalGridReportLinkStyleSheetEditorPreview }

constructor TcxVerticalGridReportLinkStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
//  inherited; cannot call ancestor method if it's an abstract method in Delphi4(5) !!!!
  FVerticalGrid := TcxVerticalGrid.Create(AOwner);
  Initialize;
end;

destructor TcxVerticalGridReportLinkStyleSheetEditorPreview.Destroy;
begin
  FVerticalGrid.Free;
  inherited;
end;

class function TcxVerticalGridReportLinkStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxVerticalGridReportLinkStyleSheet;
end;

procedure TcxVerticalGridReportLinkStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
var
  VGridStyles: TcxVerticalGridStyles;
begin
  if AStyleSheet is TcxVerticalGridReportLinkStyleSheet then
  begin
    VGridStyles := VerticalGrid.Styles;
    with TcxVerticalGridReportLinkStyleSheet(AStyleSheet).Styles do
    begin
      VGridStyles.Category := Category;
      VGridStyles.Content := Content;
      VGridStyles.Header := Header;
    end;
  end
  else
    VerticalGrid.Styles.ResetStyles;
end;

function TcxVerticalGridReportLinkStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := VerticalGrid;
end;

function TcxVerticalGridReportLinkStyleSheetEditorPreview.GetSize: TPoint;
var
  I: Integer;
begin
  Result := inherited GetSize;
  with VerticalGrid.FirstRow do
  begin
    Result.Y := Height;
    for I := 0 to Count - 1 do
      Inc(Result.Y, Height);
  end;
end;

class procedure TcxVerticalGridReportLinkStyleSheetEditorPreview.Register;
begin
  RegisterStyleSheetEditorPreview(Self);
end;

class procedure TcxVerticalGridReportLinkStyleSheetEditorPreview.Unregister;
begin
  UnregisterStyleSheetEditorPreview(Self);
end;

procedure TcxVerticalGridReportLinkStyleSheetEditorPreview.Initialize;
const
  RowCount = 7;
  RowCaptions: array[0..RowCount - 1] of string =
    (sdxLuxurySedans, sdxCarManufacturer, sdxCarModel, sdxPicture, sdxCarEngine,
     sdxCarTransmission, sdxCarTires);
  RowClasses: array[0..RowCount - 1] of TcxCustomRowClass =
    (TcxCategoryRow, TcxEditorRow, TcxEditorRow, TcxEditorRow, TcxEditorRow,
     TcxEditorRow, TcxEditorRow);
  RowEditProperties: array[0..RowCount - 1] of TcxCustomEditPropertiesClass =
    (nil, TcxTextEditProperties, TcxTextEditProperties, TcxImageProperties,
     TcxMemoProperties, TcxTextEditProperties, TcxMemoProperties);
  RowHeights: array[0..RowCount - 1] of Integer =
    (-1, -1, -1, 116, 57, -1, 44);
  RowValues: array[0..RowCount - 1] of string =
    ('', sdx760V12Manufacturer, sdx760V12Model, '', sdx760V12Engine,
     sdx760V12Transmission, sdx760V12Tires);
var
  I: Integer;
  Row: TcxCustomRow;
begin
  VerticalGrid.ClearRows;
  for I := 0 to RowCount - 1 do
  begin
    Row := VerticalGrid.Add(RowClasses[I]);
    if Row is TcxEditorRow then
      with TcxEditorRow(Row) do
      begin
        Properties.Caption := DropAmpersand(RowCaptions[I]);
        Properties.EditPropertiesClass := RowEditProperties[I];
        Properties.Value := RowValues[I];
        if Properties.EditPropertiesClass.InheritsFrom(TcxImageProperties) then
          Properties.Value := PreviewImageAsString;
        if RowHeights[I] <> -1 then Height := RowHeights[I];
        Parent := VerticalGrid.FirstRow;
      end
    else
      TcxCategoryRow(Row).Properties.Caption := DropAmpersand(RowCaptions[I]);
  end;
  VerticalGrid.OptionsData.Editing := False;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterNoIcon([TcxVerticalGridReportLink, TcxVirtualVerticalGridReportLink,
    TcxRTTIInspectorReportLink, TcxDBVerticalGridReportLink,
    TcxVerticalGridReportLinkStyleSheet]);
  dxPSRegisterReportLinkUnit('dxPScxVGridLnk', TcxDBVerticalGridReportLink);
  dxPSRegisterReportLinkUnit('dxPScxVGridLnk', TcxRTTIInspectorReportLink);
  dxPSRegisterReportLinkUnit('dxPScxVGridLnk', TcxUnboundVerticalGridReportLink);
  dxPSRegisterReportLinkUnit('dxPScxVGridLnk', TcxVerticalGridReportLink);
  // Hide moved property
  RegisterPropertyEditor(TypeInfo(Boolean), TcxVerticalGridReportLinkOptionsRefinements,
    'SuppressBackgroundBitmaps', nil);
end;

procedure RegisterStyleSheetAssistants;
begin
  cxStyles.RegisterStyleSheetClass(TcxVerticalGridReportLinkStyleSheet);
  TcxVerticalGridReportLinkStyleSheetEditorPreview.Register;
end;

procedure UnregisterStyleSheetAssistants;
begin
  TcxVerticalGridReportLinkStyleSheetEditorPreview.Unregister;
  cxStyles.UnregisterStyleSheetClass(TcxVerticalGridReportLinkStyleSheet);
end;

initialization
  RegisterStyleSheetAssistants;

finalization
  UnregisterStyleSheetAssistants;

end.
