{$INCLUDE ..\..\Compilers.Inc}

unit UserPictureWnd;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.ExtCtrls;
     {$ELSE}
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls;
     {$ENDIF}


type
  Twnd_msi_UserPicture = class(TForm)
    Image: TImage;
    procedure ImageClick(Sender: TObject);
  private
  public
  end;

var
  wnd_msi_UserPicture: Twnd_msi_UserPicture;

procedure DisplayUserPicture(AX,AY: Integer; ABitmap: TBitmap);

implementation

{$R *.dfm}

procedure DisplayUserPicture(AX,AY: Integer; ABitmap: TBitmap);
begin
  with Twnd_msi_UserPicture.Create(Application.MainForm) do
    try
      Left:=AX;
      Top:=AY;
      Image.Picture.Bitmap.Assign(ABitmap);
      Width:=Image.Picture.Bitmap.Width+Image.Margins.Left+Image.Margins.Right;
      Height:=Image.Picture.Bitmap.Height+Image.Margins.Top+Image.Margins.Bottom;
      ShowModal;
    finally
      Close;
    end;
end;

procedure Twnd_msi_UserPicture.ImageClick(Sender: TObject);
begin
  Close;
end;

end.
