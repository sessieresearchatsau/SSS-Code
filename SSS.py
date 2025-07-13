class SSS(object):
    def __init__(self, ruleSet,  Init: str, n: int, opts = None, SSSSize: int = 100, NetSize: int = 300):
        self.ruleSet = self._transform(ruleSet)
        self.Init= Init
        self.n = n
        self.opts = opts
        self.SSSSize = SSSSize
        self.NetSize = NetSize
        self.world = self.Init

    def __str__(self) -> str:
        return f"{self.world}"
    def  __repr__(self) -> str:
        return f"SSS('{self.world}')"
    def _transform(self, rules: list) -> dict:
        rule_dict = {}
        for rule in rules:
            if "->" in rule:
                src, dst = rule.split("->")
                rule_dict[src.strip()] = dst.strip()
        return rule_dict

    def worldGen(self):
        print(self.world)
        for i in range(self.n):
            for x, y in self.ruleSet.items():
                if x in self.world:
                    self.world = self.world.replace(x,y,1)
                    print(self.world)
        pass
# print(SSS(["ABA->AAB", "A->ABA"], "AB", 10).ruleSet)
# print(SSS(["ABA->AAB", "A->ABA"], "AB", 10).ruleSet['A'])
SSS(["ABA->AAB", "A->ABA"], "AB", 10).worldGen()