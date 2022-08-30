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

unit dxPSAutoRELnk;

interface

{$I cxVer.inc}

uses
  Classes, Windows, Graphics, Controls, ARichEd, dxPSCore, dxPSContainerLnk,
  dxPSRELnk;

type
  TdxPSdxCustomAutoRichEditProducer = class(TdxPSContainerCustomWinControlProducer)
  protected
    function CreateImage: TGraphic; virtual;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
    function ObjectExpandHeight: Boolean; override;
  public
    function Control: TCustomAutoRichEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;

    function ProducingObjectFriendlyName: string; override;
  end;

  TdxAutoRichEditReportLink = class(TAbstractdxRichEditReportLink)
  private
    function GetAutoRichEdit: TAutoRichEdit;
  protected
    function GetRichEditHandle: HWND; override;
  public
    constructor Create(AOwner: TComponent); override;
    property AutoRichEdit: TAutoRichEdit read GetAutoRichEdit;
  end;

implementation

type
  TCustomAutoRichEditAccess = class(TCustomAutoRichEdit);

{ TdxPSdxCustomAutoRichEditProducer }

function TdxPSdxCustomAutoRichEditProducer.Control: TCustomAutoRichEdit;
begin
  Result := inherited Control as TCustomAutoRichEdit;
end;

class function TdxPSdxCustomAutoRichEditProducer.ControlClass: TControlClass;
begin
  Result := TCustomAutoRichEdit;
end;

function TdxPSdxCustomAutoRichEditProducer.ProducingObjectFriendlyName: string;
begin
  Result := '';
  if not IsDesigning and (TCustomAutoRichEditAccess(Control).Text <> '') then
    Result := dxPSContainerLnk.dxPSMakeFriendlyNameFromStrings(TCustomAutoRichEditAccess(Control).Lines);
  if Result = '' then
    Result := inherited ProducingObjectFriendlyName;
end;

function TdxPSdxCustomAutoRichEditProducer.CreateImage: TGraphic;

  function GetMargins: TRect;
  begin
    Result := Rect(dxTextSpace, dxTextSpace, dxTextSpace, dxTextSpace);
  end;

  function GetMaxHeight: Integer;
  begin
    if Definition.OptionsPlace.ExpandHeight then
      Result := -1
    else
      Result := Control.Height;
  end;

begin
  Result := GetRichEditAsGraphic(Control.Handle, TCustomAutoRichEditAccess(Control).Color,
    GetMargins, TMetafile, Control.Width, -1, GetMaxHeight, DC);
end;

procedure TdxPSdxCustomAutoRichEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  Graphic: TGraphic;
begin
  inherited;
  with TdxReportCellGraphic(AnItem) do
  begin
    BorderClass := TdxPSCellSunkenBorder;
    CellSides := BorderStyleMap[TCustomAutoRichEditAccess(Control).BorderStyle];
    Color := TCustomAutoRichEditAccess(Control).Color;
    Graphic := Self.CreateImage;
    try
      Image := Graphic;
    finally
      Graphic.Free;
    end;
    Transparent := False;
  end;
end;

function TdxPSdxCustomAutoRichEditProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellGraphic;
end;

function TdxPSdxCustomAutoRichEditProducer.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

{ TdxAutoRichEditReportLink }

constructor TdxAutoRichEditReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RichEditVersion := 2;
  LinkModified(False);
end;

function TdxAutoRichEditReportLink.GetAutoRichEdit: TAutoRichEdit;
begin
  Result := inherited Component as TAutoRichEdit;
end;

function TdxAutoRichEditReportLink.GetRichEditHandle: HWND;
begin
  if AutoRichEdit <> nil then
    Result := AutoRichEdit.Handle
  else
    Result := 0;
end;

{ Assistants }

procedure RegisterProducers;
begin
  TdxPSdxCustomAutoRichEditProducer.Register;
end;

procedure UnregisterProducers;
begin
  TdxPSdxCustomAutoRichEditProducer.Unregister;
end;

initialization
  dxPSRegisterReportLink(TdxAutoRichEditReportLink, TAutoRichEdit, nil);
  RegisterProducers;

finalization
  UnregisterProducers;
  dxPSUnregisterReportLink(TdxAutoRichEditReportLink, TAutoRichEdit, nil);

end.

