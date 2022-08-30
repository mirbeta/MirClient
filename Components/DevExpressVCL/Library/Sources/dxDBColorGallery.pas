{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxDBColorGallery;

{$I cxVer.inc}

interface

uses
  Sysutils, Classes, cxControls, cxDB, cxDataUtils, dxGallery, dxColorGallery;

type
  { TdxDBColorGallery }

  TdxDBColorGallery = class(TdxCustomColorGallery)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(Value: TcxDBDataBinding);

    procedure DataChange;
    procedure DataSetChange;
    procedure UpdateData;
  protected
    FDataBinding: TcxDBDataBinding;

    procedure DoClickItem(AItem: TdxGalleryItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Enabled;
    property Font;
    property PopupMenu;
    property Transparent;
    property Visible;

    property AutoSizeMode default asAutoSize;
    property BorderStyle default cxcbsDefault;
    property LookAndFeel;

    property ColorPalette default cpOffice;
    property ColorSet default csDefault;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property ItemShowHint default False;
    property ItemSize default 0;
    property ShowItemBorders default True;

    // Events
    property OnChange;
    property OnGetCustomColorSet;
  end;

implementation

uses dxGalleryControl;

{ TdxDBColorGallery }

constructor TdxDBColorGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDataBinding := TcxDBDataBinding.Create(Self, Self);
  FDataBinding.OnDataChange := DataChange;
  FDataBinding.OnDataSetChange := DataSetChange;
  FDataBinding.OnUpdateData := UpdateData;
end;

destructor TdxDBColorGallery.Destroy;
begin
  FreeAndNil(FDataBinding);
  inherited;
end;

procedure TdxDBColorGallery.DoClickItem(AItem: TdxGalleryItem);
begin
  if DataBinding.SetEditMode then
  begin
    inherited;
    DataBinding.UpdateDataSource;
  end;
end;

function TdxDBColorGallery.GetDataBinding: TcxDBDataBinding;
begin
  Result := FDataBinding;
end;

procedure TdxDBColorGallery.SetDataBinding(Value: TcxDBDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TdxDBColorGallery.DataChange;
begin
  EditValue := DataBinding.GetStoredValue(evsValue, Focused)
end;

procedure TdxDBColorGallery.DataSetChange;
begin
  LayoutChanged;
end;

procedure TdxDBColorGallery.UpdateData;
begin
  DataBinding.SetStoredValue(evsValue, EditValue);
end;

end.
