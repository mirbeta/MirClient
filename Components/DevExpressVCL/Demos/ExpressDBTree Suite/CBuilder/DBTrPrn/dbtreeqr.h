//----------------------------------------------------------------------------
#ifndef dbtreeqrH
#define dbtreeqrH
//----------------------------------------------------------------------------
#include <ExtCtrls.hpp>
#include <StdCtrls.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Controls.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <System.hpp>
#include <DB.hpp>
#include <Qrctrls.hpp>
#include <quickrpt.hpp>
//----------------------------------------------------------------------------
class TQRListForm : public TForm
{
__published:
    TQuickRep *QuickReport;
    TQRBand *PageHeader;
    TQRSysData *PageNumber;
    TQRBand *Title;
    TImage *Image2;
    TQRLabel *QRLabel1;
    TQRBand *Detail;
    TQRDBText *QText;
    TQRImage *Image;
    TQRImage *ImageRect;
	void __fastcall DataSource1DataChange(TObject *Sender, TField *Field);
    void __fastcall QuickReportBeforePrint(TCustomQuickRep *Sender,
          bool &PrintReport);
    void __fastcall QuickReportAfterPrint(TObject *Sender);
    void __fastcall DetailAfterPrint(TQRCustomBand *Sender,
          bool BandPrinted);
    void __fastcall DetailBeforePrint(TQRCustomBand *Sender,
          bool &PrintBand);
private:
   int ShapeCount;
   TList* ShapeList;
   int ImageLeft, QTextLeft, ImageRectLeft;
   Graphics::TBitmap* bmp;
public:
	virtual __fastcall TQRListForm(TComponent* AOwner);
};
//----------------------------------------------------------------------------
extern TQRListForm *QRListForm;
//----------------------------------------------------------------------------
#endif
