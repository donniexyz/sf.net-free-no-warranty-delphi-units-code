OVERVIEW :  
********

TwhForm2HTML is a WYSIWYG Delphi form to HTML translator. 

Drag and drop a TwhForm2HTML component on any Delphi form and you are ready to export it in HTML.

A custom editor gives you full control on the HTML attributes used, plus a few options.

Exports can be done :

- in standard HTML or as WebHub chunks
- at design time or dynamically at run time.

As browsers size objects quite differently, the component may address the four main browsers differently in order to keep a common look and feel despite of their different behaviours.

Who is the ideal user ? 

- The Delphi developer which has an HTML form to code and doesn't master HTML too much - or simply wants to save time - can use the component at *design time* only, and export the HTML code to integrate it in a static page. By letting the BrowserFamily property to bfDefault, the component chooses a mean way in sizing objects which gives an acceptable rendering with all browsers.

- The Delphi developer which has an existing C/S Delphi application to be ported for remote access through the Web, with tens of data entry screens can do it in a breeze.
By adding the component to all the forms for which a remote access is needed and including those forms in a WebHub project, a big part of the job is got immediately. In that way of use, keeping the original look and feel of the form is a big plus of course.

SUPPORTED CONTROLS :
******************

- TLabel
- TEdit
- TMemo
- TCheckBox
- TRadioButton
- TListBox
- TComboBox
- TButton
- TImage
- TGroupBox
- TPanel
- and any other TWinControl container.

AT DESIGN-TIME :
**************

PROPERTIES FROM THE OBJECT INSPECTOR :
************************************

AlternateFonts : string	= Optional string appended to the 'Face' attribute of the <FONT> tags.
Attributes...		= Open the editor (see below)
BGColor : string	= Fixes the form's background color. Accepts :
			  * [None] : no background color
			  * [Same] : same background color as the Delphi form color
			  * any HTML color name
			  * any HTML color string in the form #RRGGBB
                          * any color picked in a TColorDialog (opened by double clicking)
Browse it !		= Opens the HTML form using your default browser. BrowserFamily should
			  be set before, according to that browser.
BrowserFamily		= (bfDefault, bfIE3, bfIE4, bfNS3, bfNS4). Fixes the target browser.
			  bfDefault equals to bfIE4 because of the mean way of IE4 in sizing 
			  objects.
ChunkOnly : Boolean	= When exporting in standard mode if ChunkOnly is False, the component
			  exports a whole HTML page, including a page title.
Export HTML...		= Opens a TSaveDialog to perform the export in standard HTML mode
Export WebHub chunk...	= Opens a TSaveDialog to perform the export in the WebHub mode
MainContainer : 
	TWinControl	= By default, TwhForm2HTML converts the whole form. By setting 
			  MainContainer to any other container (groupbox, panel, ...), you may
			  restrict the export to that container.
SetFonts : Boolean	= If True, TwhForm2HTML uses the <FONT> tag to keep the elements fonts.
                          The controls which are concerned are : labels, radiobuttons and
			  checkboxes

THE ATTRIBUTES EDITOR :
*********************

Opened by double clicking on the component, the editor lists the controls in a tree view on the left panel. By selecting a control in the tree, you may check / edit its attributes on the right panel. The first element in the tree (<FORM>..</FORM>) gives you control on the <FORM> tag attributes.

The editor shows three kinds of attributes :

- HTML attributes
- HTML attributes used when exporting in WebHub mode : they have the same name as the standard 
  attribute, prefixed by 'WH'. Example : Action <--> WHAction.
- TwhForm2HTML options

Only the TwhForm2HTML options are documented here. They vary depending on the control selected :

Options for all elements :
- AfterElement : string		= Lets you insert any HTML code after the element
- BeforeElement : string	= Lets you insert any HTML code before the element

Options for all elements but containers and labels :
- ExtraAttributes		= To define additionals HTML attributes (like scripts handlers
				  for example) under the form :
				  'AnAttribute1="Value1" AnAttributeN="ValueN"'

Options for a TForm :
- Border : Boolean		= If True, the form is rendered with a border around it
- ShowCaption : Boolean		= If True, the title bar of the form is kept
- CaptionBGColor : string	= Back color of the title bar if ShowCaption is True
				  Default = 'Navy'
- CaptionColor : string		= Text color of the title bar if ShowCaption is True
				  Default = 'White'

Options for a TButton :
- AsImage : Boolean		= Specifies if the button has to be converted in an <INPUT>
				  tag (standard HTML button, type "submit" or "reset"), or in 
				  an <IMG> tag.

Options for a TImage :
- AsButton : Boolean		= If True, the TImage is translated in an <INPUT Type="image">
				  tag. If False, the TImage can be handled as an anchor or not.
- AsAnchor : Boolean		= If True, you can edit the HREF link attribute.

AT RUN-TIME :
***********

The component can be used to generate the forms chunks on the fly. A write-only 'CGIUserAgent' property, compatible with TWebServer.CGIUserAgent must be used to inform the component of the target browser.

Example in WebHub mode :

with WHForm2HTML1 do begin
   CGIUserAgent := WebServer1.CGIUserAgent;   // set BrowserFamily automatically
   ExportWHChunkFName := 'aChunk.txt';	      // set the export filename
   if exportWHChunk then		      // function exportWHChunk : Boolean
      // do something with the text file
end;

In place of writing the chunk in a text file, you can get the chunk string through the public
property WHChunk : string read getWHChunk.

Example in standard mode :

with WHForm2HTML1 do begin
   CGIUserAgent := WebServer1.CGIUserAgent;   // sets BrowserFamily automatically
   ExportFName := 'aForm.HTML';	      	      // sets the export filename
   if exportHTML then		      	      // function exportHTML : Boolean
      // do something with the HTML page
end;

In place of writing the page in a text file, you can get the its string through the public
property HTML : string read getHTML.

Notice that if you create controls at run-time, TWHForm2HTML must be informed in order to take those new controls into account. To achieve that, simply call the Synchronize procedure.
As shown below, it's also possible to set attributes on-the-fly.

Example :

var
   attrObject : THAttrObject;
begin
with WHForm2HTML1 do begin
   // Creation of controls on the fly
   // ...
   // ...
   synchronize;   // Now the component knows about them
   // ...
   // Now let's say that you created a TButton named 'Button1' and you
   // need to access the corresponding THButton object in order to set
   // one of its HTML attributes :

   attrObject := WHForm2HTML1.HMainContainer.FindHObjectByName('Button1');
   if attrObject <> nil then begin
      with attrObject do begin
         Attributes['AsImage'].BooleanValue := False;   // set the 'AsImage' option
         Attributes['Type'].Value := 'reset';           // set the 'Type' attribute
      end;
   end;
end;   


INSTALLATION :
************

TWHForm2HTML currently supports Delphi 5+ and C++ Builder.

WebHub VCL Developers :
---------------------
- The trial version of F2h is automatically installed on the WebHub palette 
when you build your component library or package.
- To install the registered version, backup and then replace the 
following files in ht\lib32 : f2h.* and regf2h.pas.
- Rebuild your component library or package to take advantage of your 
new code.  


Delphi 5, 6, 7 and C++ Builder :
---------------------------------

- Unzip the ZIP file in any folder
- Menu: Component > Install Packages
- Install the package for your compiler, e.g. f2hLib_d11_win32 for Delphi 2007

D06 = Delphi 6
D07 = Delphi 7
D11 = Delphi 2007
D12 = Delphi 2009
D14 = Delphi 2010
D15 = Delphi XE

The component will be installed on a palette named "WebHub".



TO CONTRIBUTE, please donate online via 
the Web: http://www.href.com/hrefshop:detail::0117


THANK YOU!

--
Copyright (c) 1998-2010 HREF Tools Corp. All Rights Reserved.
