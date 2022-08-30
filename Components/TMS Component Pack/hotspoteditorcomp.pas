{*************************************************************************}
{ THotSpotImage component                                                 }
{ for Delphi  & C++Builder                                                }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2010                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit HotSpotEditorComp;

interface

uses
  Classes, SysUtils, Controls, HotSpotImage, HotSpotEditor;

type

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THotSpotEditor = class(TComponent)
  private
    FHotSpotImage: THotSpotImage;
  
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    function Execute: boolean;
  published
    property HotSpotImage: THotSpotImage read FHotSpotImage write FHotSpotImage;
  end;



implementation


{ THotSpotEditor }

function THotSpotEditor.Execute: boolean;
var
  frmEditor: TfrmHSIEditor;
begin
  if not Assigned(HotSpotImage) then
    raise Exception.Create('No HotSpotImage assigned');

  Result := false;
  FHotSpots.Assign(HotSpotImage.HotSpots);
  FPicture.Assign(HotSpotImage.Picture);
  frmEditor := TfrmHSIEditor.Create(nil);
  try
    if frmEditor.ShowModal = mrOK then
    begin
      HotSpotImage.HotSpots.Assign(FHotSpots);
      HotSpotImage.Picture.Assign(FPicture);
      Result := true;
    end;
  finally
    frmEditor.Free;
  end;
end;

procedure THotSpotEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FHotSpotImage) then
    FHotSpotImage := nil;
end;

end.
