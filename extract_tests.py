import os
from bs4 import BeautifulSoup


def extract_tests(html_file, output_dir):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    with open(html_file, 'r') as file:
        soup = BeautifulSoup(file, 'html.parser')

    pre_blocks = soup.find_all('pre')
    for i, pre in enumerate(pre_blocks, start=1):
        # Strip leading and trailing whitespace
        test_code = pre.get_text().strip()  
        test_file = os.path.join(output_dir, f'P4test{i}.txt')
        with open(test_file, 'w') as file:
            file.write(test_code)


if __name__ == "__main__": 
    html_file = 'part4tests.html'
    output_dir = 'tests'
    extract_tests(html_file, output_dir)
