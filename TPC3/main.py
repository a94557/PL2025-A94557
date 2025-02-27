import re

def markdown_to_html(md_text):
    # Cabeçalhos
    md_text = re.sub(r'### (.+)', r'<h3>\1</h3>', md_text)
    md_text = re.sub(r'## (.+)', r'<h2>\1</h2>', md_text)
    md_text = re.sub(r'# (.+)', r'<h1>\1</h1>', md_text)
    
    # Bold
    md_text = re.sub(r'\*\*(.*?)\*\*', r'<b>\1</b>', md_text)
    
    # Itálico
    md_text = re.sub(r'\*(.*?)\*', r'<i>\1</i>', md_text)
    
    # Listas numeradas
    md_text = re.sub(r'\n\d+\. (.+)', r'\n<li>\1</li>', md_text)
    md_text = re.sub(r'(?:<li>.*?</li>\n?)+', lambda m: f"<ol>{m.group(0)}</ol>", md_text, flags=re.S)
    
    # Imagens
    md_text = re.sub(r'!\[(.*?)\]\((.*?)\)', r'<img src="\2" alt="\1"/>', md_text)

    # Links
    md_text = re.sub(r'\[(.*?)\]\((.*?)\)', r'<a href="\2">\1</a>', md_text)
    
    
    return md_text

def convert_md_to_html(input_filename, output_filename):
    with open(input_filename, 'r', encoding='utf-8') as md_file:
        md_content = md_file.read()
    
    html_content = markdown_to_html(md_content)
    
    with open(output_filename, 'w', encoding='utf-8') as html_file:
        html_file.write(html_content)

# Exemplo
input_file = "entrada.md"
output_file = "saida.html"
convert_md_to_html(input_file, output_file)
print(f"File HTML: {output_file}")
