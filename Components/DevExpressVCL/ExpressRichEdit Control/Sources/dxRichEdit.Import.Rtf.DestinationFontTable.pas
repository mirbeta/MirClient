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

unit dxRichEdit.Import.Rtf.DestinationFontTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses, dxRichEdit.Import.Rtf;

type
  TdxFontTableDestination = class(TdxRichEditRtfDestinationBase)
  private
    FEmptyFontInfo: Boolean;
    FFontInfo: TdxRtfFontInfo;
    FNestedState: Boolean;
    procedure AddFontInfo;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    function ProcessKeywordCore(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;

    procedure ProcessCharCore(AChar: Char); override;
    procedure FontCharsetHandler(AParameterValue: Integer);

    property NestedState: Boolean read FNestedState;
  public
    constructor Create(AImporter: TdxRtfImporter; ANestedState: Boolean = False); reintroduce;
    destructor Destroy; override;

    procedure AfterPopRtfState; override;
    procedure BeforePopRtfState; override;
  end;

implementation

uses
  dxRichEdit.Platform.Font,
  dxEncoding,
  dxStringHelper;

constructor TdxFontTableDestination.Create(AImporter: TdxRtfImporter; ANestedState: Boolean = False);
begin
  inherited Create(AImporter);
  FNestedState := ANestedState;
  FFontInfo := TdxRtfFontInfo.Create;
  FEmptyFontInfo := True;
end;

function TdxFontTableDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxFontTableDestination.Create(Importer, True);
end;

destructor TdxFontTableDestination.Destroy;
begin
  FreeAndNil(FFontInfo);
  inherited;
end;

function TdxFontTableDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
begin
  if not AHasParameter then
    AParameterValue := 0;
  Result := True;
  FEmptyFontInfo := False;
  if AKeyword = 'f' then
    FFontInfo.ID := AParameterValue
  else
    if AKeyword = 'fcharset' then
      FontCharsetHandler(AParameterValue)
    else
      if AKeyword = 'bin' then
        Result := inherited ProcessKeywordCore(AKeyword, AParameterValue, AHasParameter)
      else
        Result := False;
end;

procedure TdxFontTableDestination.AddFontInfo;
begin
  if FNestedState and FEmptyFontInfo then
    Exit;

  FFontInfo.Name := TdxStringHelper.RemoveSpecialSymbols(FFontInfo.Name);
  if Length(FFontInfo.Name) = 0 then
    FFontInfo.Name := Importer.DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem.FontName;

  Importer.DocumentProperties.Fonts.Add(FFontInfo);
  FFontInfo := TdxRtfFontInfo.Create;
  FEmptyFontInfo := True;
end;

procedure TdxFontTableDestination.ProcessCharCore(AChar: Char);
begin
  if AChar = ';' then
  begin
    AddFontInfo;
    Importer.SetCodePage(Importer.DocumentProperties.DefaultCodePage);
  end
  else
  begin
    FFontInfo.Name := FFontInfo.Name + AChar;
    FEmptyFontInfo := False;
  end;
end;

procedure TdxFontTableDestination.FontCharsetHandler(AParameterValue: Integer);
begin
  FFontInfo.Charset := AParameterValue;
  if FFontInfo.Charset >= 0 then
    Importer.SetCodePage(TdxEncoding.CodePageFromCharset(FFontInfo.Charset));
end;

procedure TdxFontTableDestination.AfterPopRtfState;
var
  AFontInfo: TdxRtfFontInfo;
  AProperties: TdxRtfDocumentProperties;
begin
  AProperties := Importer.DocumentProperties;
  AFontInfo := AProperties.Fonts.GetRtfFontInfoById(AProperties.DefaultFontNumber);
  Importer.Position.CharacterFormatting.FontName := AFontInfo.Name;
  if (AFontInfo <> AProperties.Fonts.DefaultRtfFontInfo) and (AFontInfo.Charset >= 0) then
    Importer.DocumentProperties.DefaultCodePage := TdxEncoding.CodePageFromCharset(AFontInfo.Charset);
  Importer.SetCodePage(Importer.DocumentProperties.DefaultCodePage);
end;

procedure TdxFontTableDestination.BeforePopRtfState;
begin
  if FNestedState and not FEmptyFontInfo then
    AddFontInfo;
  inherited BeforePopRtfState;
end;

end.
