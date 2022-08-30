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

unit dxRichEdit.Utils.FileDialogFilter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxGenerics;

type
  { TdxFileDialogFilter }

  TdxFileDialogFilter = class
  private class var
    FAllFiles: TdxFileDialogFilter;
    FEmpty: TdxFileDialogFilter;
    FRtfFiles: TdxFileDialogFilter;
    FOpenXMLFiles: TdxFileDialogFilter;
    FHtmlFiles: TdxFileDialogFilter;
    FDocFiles: TdxFileDialogFilter;
  strict private
    FDescription: string;
    FExtensions: TStrings;
  private
    class constructor Initialize;
    class destructor Finalize;
  protected
    procedure AppendExtensions(AStringBuilder: TStringBuilder); virtual;
    procedure AppendExtension(AStringBuilder: TStringBuilder; const AExtension: string); virtual;
    function CreateFilterString: string; virtual;
  public
    constructor Create; overload;
    constructor Create(const ADescription, AExtension: string); overload;
    constructor Create(const ADescription: string; const AExtensions: array of string); overload;
    constructor Create(const ADescription: string; AExtensions: TStrings); overload;
    destructor Destroy; override;

    function Clone: TdxFileDialogFilter;

    function ToString: string; override;

    class property AllFiles: TdxFileDialogFilter read FAllFiles;
    class property Empty: TdxFileDialogFilter read FEmpty;
    class property RtfFiles: TdxFileDialogFilter read FRtfFiles;
    class property OpenXMLFiles: TdxFileDialogFilter read FOpenXMLFiles;
    class property HtmlFiles: TdxFileDialogFilter read FHtmlFiles;
    class property DocFiles: TdxFileDialogFilter read FDocFiles;

    property Description: string read FDescription write FDescription;
    property Extensions: TStrings read FExtensions;
  end;

  { TdxFileDialogFilterCollection }

  TdxFileDialogFilterCollection = class(TdxObjectList<TdxFileDialogFilter>)
  public
    function CreateFilterString: string;
    function ToString: string; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Strs;

{ TdxFileDialogFilter }

constructor TdxFileDialogFilter.Create(const ADescription, AExtension: string);
begin
  Create;
  FDescription := ADescription;
  FExtensions.Add(AExtension);
end;

constructor TdxFileDialogFilter.Create;
begin
  inherited Create;
  FExtensions := TStringList.Create;
end;

constructor TdxFileDialogFilter.Create(const ADescription: string; const AExtensions: array of string);
var
  AExtension: string;
begin
  Create;
  FDescription := ADescription;
  for AExtension in AExtensions do
    FExtensions.Add(AExtension);
end;

destructor TdxFileDialogFilter.Destroy;
begin
  FreeAndNil(FExtensions);
  inherited Destroy;
end;

procedure TdxFileDialogFilter.AppendExtension(AStringBuilder: TStringBuilder;
  const AExtension: string);
begin
  AStringBuilder.Append('*.');
  AStringBuilder.Append(AExtension);
end;

procedure TdxFileDialogFilter.AppendExtensions(AStringBuilder: TStringBuilder);
var
  I: Integer;
begin
  AppendExtension(AStringBuilder, Extensions[0]);
  for I := 1 to Extensions.Count - 1 do
  begin
    AStringBuilder.Append('; ');
    AppendExtension(AStringBuilder, Extensions[I]);
  end;
end;

function TdxFileDialogFilter.Clone: TdxFileDialogFilter;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxFileDialogFilter.Create(Description, Extensions);
end;

constructor TdxFileDialogFilter.Create(const ADescription: string;
  AExtensions: TStrings);
begin
  Create;
  FDescription := ADescription;
  FExtensions.AddStrings(AExtensions);
end;

function TdxFileDialogFilter.CreateFilterString: string;
var
  AStringBuilder: TStringBuilder;
begin
  AStringBuilder := TStringBuilder.Create;
  try
    AStringBuilder.Append(Description);
    AStringBuilder.Append(' (');
    AppendExtensions(AStringBuilder);
    AStringBuilder.Append(')|');
    AppendExtensions(AStringBuilder);
    Result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;

class constructor TdxFileDialogFilter.Initialize;
begin
  FAllFiles := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_AllFiles), '*');
  FEmpty := TdxFileDialogFilter.Create;
  FOpenXMLFiles := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_OpenXmlFiles), 'docx');
  FRtfFiles := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_RtfFiles), 'rtf');
  FHtmlFiles := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_HtmlFiles), ['htm', 'html']);
  FDocFiles := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_DocFiles), ['doc']);
end;

class destructor TdxFileDialogFilter.Finalize;
begin
  FreeAndNil(FAllFiles);
  FreeAndNil(FEmpty);
  FreeAndNil(FRtfFiles);
  FreeAndNil(FOpenXMLFiles);
  FreeAndNil(FHtmlFiles);
  FreeAndNil(FDocFiles);
end;

function TdxFileDialogFilter.ToString: string;
begin
  if Extensions.Count <= 0 then
    Result := AllFiles.ToString;
  Result := CreateFilterString;
end;

{ TdxFileDialogFilterCollection }

function TdxFileDialogFilterCollection.CreateFilterString: string;
var
  AStringBuilder: TStringBuilder;
  I: Integer;
begin
  if Count <= 0 then
    Exit(EmptyStr);

  AStringBuilder := TStringBuilder.Create;
  try
    AStringBuilder.Append(Items[0].ToString);
    for I := 1 to Count - 1 do
    begin
      AStringBuilder.Append('|');
      AStringBuilder.Append(Items[I].ToString);
    end;
    Result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;

function TdxFileDialogFilterCollection.ToString: string;
begin
  Result := CreateFilterString;
end;

end.
