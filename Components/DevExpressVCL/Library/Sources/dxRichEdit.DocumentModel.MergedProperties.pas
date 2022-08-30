{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.DocumentModel.MergedProperties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCoreClasses,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.Utils.Types;

type

  { TdxMergedProperties }

  TdxMergedProperties<TInfo: TdxCloneable, constructor; TOptions> = class(TcxIUnknownObject)
  private
    FInfo: TInfo;
  protected
    FOptions: TOptions;
  public
    constructor Create; overload;
    constructor Create(AInfo: TInfo; const AOptions: TOptions); overload;
    destructor Destroy; override;

    property Info: TInfo read FInfo;
    property Options: TOptions read FOptions write FOptions;
  end;

  { TdxMergedCloneableOptionsProperties }

  TdxMergedCloneableOptionsProperties<TInfo: TdxCloneable, constructor; TOptions: TdxCloneable, constructor> = class(TdxMergedProperties<TInfo, TOptions>)
  public
    constructor Create; reintroduce; overload;
    constructor Create(AInfo: TInfo; const AOptions: TOptions); reintroduce; overload;
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils;

{ TdxMergedProperties<TInfo, TOptions> }

constructor TdxMergedProperties<TInfo, TOptions>.Create(AInfo: TInfo;
  const AOptions: TOptions);
begin
  inherited Create;
  FInfo := TInfo(AInfo.Clone);
  FOptions := AOptions;
end;

constructor TdxMergedProperties<TInfo, TOptions>.Create;
begin
  inherited Create;
  FInfo := TInfo.Create;
end;

destructor TdxMergedProperties<TInfo, TOptions>.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

{ TdxMergedCloneableOptionsProperties<TInfo, TOptions> }

constructor TdxMergedCloneableOptionsProperties<TInfo, TOptions>.Create;
begin
  inherited Create;
  FOptions := TOptions.Create;
end;

constructor TdxMergedCloneableOptionsProperties<TInfo, TOptions>.Create(
  AInfo: TInfo; const AOptions: TOptions);
begin
  inherited Create(AInfo, AOptions.Clone);
end;

destructor TdxMergedCloneableOptionsProperties<TInfo, TOptions>.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

end.
