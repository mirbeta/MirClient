{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxMapInformationProvidersEditor;

interface

{$I cxVer.inc}

uses
  Types,
  DesignIntf, DesignWindows, ComponentDesigner, DesignConst, DesignEditors, ColnEdit,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxComponentCollectionEditor,
  ImgList, cxGraphics, Menus, ActnList,
  ComCtrls, ToolWin, ExtCtrls, dxMapControlInformationProvider;

type
  TfrmMapControlInformationProvidersEditor = class(TfrmComponentCollectionEditor)
  private
    function GetInformationProviders: TdxMapControlInformationProviders;
  protected
    procedure AddItem(AItemClassIndex: Integer); override;
    procedure InitFormEditor; override;
  public
    property InformationProviders: TdxMapControlInformationProviders read GetInformationProviders;
  end;


implementation

uses
  dxMapControlBingMapInformationProviders;

const
  AInformationProvidersCount = 4;
  AInformationProvidersClasses: array [0..AInformationProvidersCount - 1] of TdxMapControlInformationProviderClass = (
    TdxMapControlBingMapGeoCodingDataProvider, TdxMapControlBingMapReverseGeoCodingDataProvider,
    TdxMapControlBingMapRouteDataProvider, TdxMapControlBingMapMajorRoadRouteDataProvider);
  AProviderDisplayNames: array [0..AInformationProvidersCount - 1] of string = (
    'Bing Maps Geocoding Provider', 'Bing Maps Reverse Geocoding Provider',
    'Bing Maps Route Provider', 'Bing Maps Major Road Route Provider');

{$R *.dfm}

{ TTfrmMapControlInformationProvidersEditor }

procedure TfrmMapControlInformationProvidersEditor.AddItem(
  AItemClassIndex: Integer);
begin
  InformationProviders.Add(AInformationProvidersClasses[AItemClassIndex]);
end;

function TfrmMapControlInformationProvidersEditor.GetInformationProviders: TdxMapControlInformationProviders;
begin
  Result := Collection as TdxMapControlInformationProviders;
end;

procedure TfrmMapControlInformationProvidersEditor.InitFormEditor;
var
  ARoot, AMenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  ARoot := pmItemClasses.Items;
  for I := 0 to High(AInformationProvidersClasses) do
  begin
    AMenuItem := TMenuItem.Create(ARoot);
    AMenuItem.Tag := I;
    AMenuItem.Caption := AProviderDisplayNames[I];
    AMenuItem.OnClick := acAddExecute;
    ARoot.Add(AMenuItem);
  end;
end;

end.
