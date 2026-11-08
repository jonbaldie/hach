import unittest
from slugify import slugify
class SlugTests(unittest.TestCase):
    def test_case_and_spaces(self): self.assertEqual(slugify(" Hello World "), "hello-world")
    def test_punctuation(self): self.assertEqual(slugify("One...two___three"), "one-two-three")
    def test_empty(self): self.assertEqual(slugify("---"), "")
    def test_digits(self): self.assertEqual(slugify("v2 Release 42"), "v2-release-42")
if __name__ == "__main__": unittest.main()
